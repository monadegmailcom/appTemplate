-- disable warning concerning Redis.PortID, I cannot do anything about it
{-# OPTIONS_GHC -Wno-deprecations #-}

module Tests.AppSpec where

import qualified App
import qualified Config
import qualified Effect.CmdLine as CmdLine
import qualified Effect.CmdLine.Impl as CmdLine ()
import qualified Effect.Database.Impl.Redis as Redis
import qualified Effect.Filesystem as Filesystem
import qualified Effect.Filesystem.Impl ()
import qualified Effect.Log as Log
import qualified Effect.Log.Impl.List as List
import           Effect.Signal.Impl ()
import qualified Effect.State.Impl as State
import           Effect.Thread.Impl ()
import qualified Helper.Redis

import qualified Control.Concurrent.Async as CA
import qualified Control.Exception as E
import           Control.Monad ((>=>))
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (asks, runReaderT, ReaderT)
import           Data.Bifunctor (first)
import qualified Data.Ini as Ini
import qualified Data.HashMap.Strict as HashMap
import           Data.IORef
import           Data.Maybe (fromMaybe, isJust)
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Database.Redis as R
import qualified System.Environment
import qualified System.Posix.Signals as PS
import qualified System.Process as Process
import           Test.Hspec
import qualified Time.Units

-- test context
data Env = Env { envState :: State.Resource
               , envLog :: IORef List.Resource
               , envRedis :: Redis.Resource
               , envConfigPath :: FilePath
               , envRedisProcessHandle :: Process.ProcessHandle
               , envAppAsync :: Maybe (CA.Async ())
               , envPatchIni :: Ini.Ini -> Ini.Ini
               }

type App = ReaderT Env IO

instance List.HasResource App where getResource = asks envLog
instance State.HasResource App where getResource = asks envState
instance Redis.HasResource App where getResource = asks envRedis

-- tweak cnnfig
instance {-# OVERLAPS #-} Filesystem.FilesystemM App where
    readFile path = do
        patchIni <- asks envPatchIni
        ini <- liftIO $ T.readFile path >>= either error return . Ini.parseIni
        return . TL.fromStrict . Ini.printIni . patchIni $ ini

patchKey :: T.Text -> T.Text -> (T.Text -> T.Text) -> Ini.Ini -> Ini.Ini
patchKey section key modifyValue ini = Ini.Ini  (HashMap.map HashMap.toList adj) []
  where
    adj = HashMap.adjust (HashMap.adjust modifyValue key) section . Ini.unIni $ ini

patchPort :: T.Text -> Ini.Ini -> Ini.Ini
patchPort port = patchKey "Redis" "url" replacePort
  where
    replacePort url = T.dropWhileEnd (`elem` (':' : ['0' .. '9'])) url <> ":" <> port

patchHost :: T.Text -> Ini.Ini -> Ini.Ini
patchHost host = patchKey "Redis" "url" (const host)

spec :: Spec
spec = context "App" $
    context "with environment set up" $ beforeAll setup . afterAll cleanup $ do
        context "with config file not found" -- invalidate config path
            $ beforeWith (\env -> return $ env { envConfigPath = "invalidPath" }) $
            context "with application ran"
                    $ beforeWith (startApplication >=> waitForApplicationDone) $
                it "throws" $ \(asyncResult, _) ->
                    first E.fromException asyncResult `shouldBe` Left (Just $ App.AppException
                        "Read config file: invalidPath: openFile: does not exist \
                        \(No such file or directory)")
        context "with invalid config file format"
            $ beforeWith (\env -> return $ env { envConfigPath = "test/fixtures/invalid.ini" }) $
            context "with application ran"
                    $ beforeWith (startApplication >=> waitForApplicationDone) $
                it "throws" $ \(asyncResult, _) ->
                    either (maybe "" App.exceptionMsg . E.fromException)
                           (const "")
                           asyncResult
                       `shouldStartWith` "Parse config file:"
        context "with invalid redis port" -- invalidate port number
             $ beforeWith (\env -> return $ env { envPatchIni = patchPort "1" }) $
            context "with application ran"
                    $ beforeWith (startApplication >=> waitForApplicationDone) $
                it "logs redis error message" $ \(asyncResult, logEntries) -> do
                    either (maybe "" App.exceptionMsg . E.fromException)
                           (const "")
                           asyncResult
                       `shouldStartWith` "Initialize database: Network.Socket.connect:"
                    logEntries `shouldSatisfy` (not . null)
                    (TL.unpack . snd . last) logEntries `shouldStartWith`
                        "AppException {exceptionMsg = \"Initialize database: \
                        \Network.Socket.connect:"
        context "with invalid redis host" -- invalidate host
             $ beforeWith (\env -> return $ env { envPatchIni = patchHost "redis://unknownHost" }) $
            context "with application ran"
                    $ beforeWith (startApplication >=> waitForApplicationDone) $
                it "throws redis exception" $ \(asyncResult, _) ->
                    case first E.fromException asyncResult of
                        Left (Just (App.AppException msg)) -> msg `shouldContain` 
                            "does not exist (Name or service not known)"
                        _ -> fail "Not an AppException thrown"
        context "with valid config" $ beforeWith return $
            context "with application started" $ beforeWith startApplication $
                context "with USR1 and INT signals sent" $ beforeWith
                    (\env -> mapM_ PS.raiseSignal [PS.sigUSR1, PS.sigINT] >> return env) $
                    context "with application shutdown" $ beforeWith waitForApplicationDone $
                        it "logs as expected" $ \(asyncResult, logEntries) -> do
                            asyncResult `shouldSatisfy`
                                either (isJust @E.SomeAsyncException . E.fromException)
                                       (return False)
                            logEntries `shouldSatisfy` (not . null)
                            (TL.unpack . snd . head) logEntries `shouldStartWith`
                                "Startup version"
                            (TL.unpack . snd . last) logEntries `shouldBe`
                                "Shutdown"
                            -- see http://man7.org/linux/man-pages/man7/signal.7.html
                            logEntries `shouldSatisfy` ((Log.Info, "Caught signal 10, ignore") `elem`)
  where
    waitForApplicationDone env = do
        -- wait for application to finish
        asyncResult <- E.try @E.SomeException
                     $ CA.wait (fromMaybe (error "Async not set") $ envAppAsync env)

        -- return log entries in chronological order
        logEntries <- reverse . List.resourceSink <$> readIORef (envLog env)
        return (asyncResult, logEntries)
    startApplication env = do
        -- reset resources
        atomicWriteIORef (envLog env) (List.Resource [] Log.Debug)
        -- start application asynchronously
        appAsync <- CA.async . System.Environment.withArgs ["-c", envConfigPath env]
                             $ runReaderT App.app env
        -- wait some time for the app to start properly and the signalhandlers are installed
        Time.Units.threadDelay $ Time.Units.sec 0.1
        return $ env { envAppAsync = Just appAsync }

-- setup environment
setup :: IO Env
setup = do
    config <- System.Environment.withArgs ["-c", filePath] $
                     CmdLine.parseCommandLineOptions
                 >>= T.readFile . CmdLine.cmdLineConfigFile
                 >>= either error (return . fst) . Config.parseIniFile
    -- patch redis connect info with free port
    let connectInfo = Config.redisConnectInfo . Config.configRedis $ config
    port <- Helper.Redis.getFreePort . R.connectHost $ connectInfo
    let patchedConnectInfo = connectInfo { R.connectPort = R.PortNumber port }
    processHandle <- Helper.Redis.startServer patchedConnectInfo
    state <- State.defaultResource
    redis <- Redis.def
    log' <- newIORef $ List.Resource [] Log.Debug
    return $ Env state log' redis filePath processHandle Nothing (patchPort . T.pack . show $ port)
  where
    filePath = "test/fixtures/valid.ini"

-- cleanup context
cleanup :: Env -> IO ()
cleanup = Process.terminateProcess . envRedisProcessHandle -- send SIGTERM

