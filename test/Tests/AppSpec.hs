-- disable warning concerning Redis.PortID, I cannot do anything about it
{-# OPTIONS_GHC -Wno-deprecations #-}

module Tests.AppSpec where

import qualified App
import qualified Config
import qualified Effect.CmdLine as CmdLine
import qualified Effect.CmdLine.Impl as CmdLine ()
import qualified Effect.Database.Impl.Redis as Redis
import qualified Effect.Database.Init as Database
import qualified Effect.Filesystem.Impl ()
import qualified Effect.Log as Log
import qualified Effect.Log.Impl.List as List
import           Effect.Signal.Impl ()
import qualified Effect.State.Impl as State
import           Effect.Thread.Impl ()
import qualified Helper.Redis

import qualified Control.Concurrent as C
import qualified Control.Concurrent.Async as CA
import qualified Control.Exception as E
import           Control.Monad ((>=>), void)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (asks, runReaderT, ReaderT)
import           Data.Bifunctor (first)
import           Data.Maybe (fromMaybe, isJust)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as T
import qualified Database.Redis as R
import qualified System.Environment
import qualified System.Posix.Signals as PS
import qualified System.Process as Process
import           Test.Hspec
import qualified Time.Units

-- test context
data Env = Env { envState :: State.Resource
               , envLog :: C.MVar List.Resource
               , envRedis :: C.MVar Redis.Resource
               , envConfigPath :: FilePath
               , envRedisProcessHandle :: Process.ProcessHandle
               , envAppAsync :: Maybe (CA.Async ())
               , envConnectInfo :: R.ConnectInfo
               }

type App = ReaderT Env IO

instance List.HasResource App where getResource = asks envLog
instance State.HasResource App where getResource = asks envState
instance Redis.HasResource App where getResource = asks envRedis

-- overwrite database redis initialization with port patched by a dynamically requested free port
instance {-# OVERLAPS #-} Database.InitM App where
    init config = do
        -- patch connect info
        connectInfo <- asks envConnectInfo
        let patchedConfig = config { Config.redisConnectInfo = connectInfo }
        connection <- liftIO $ R.checkedConnect (Config.redisConnectInfo patchedConfig)
        asks envRedis >>= liftIO . (`C.putMVar` Redis.Resource connection patchedConfig)

spec :: Spec
spec = context "App" $
    context "with environment set up" $ beforeAll setup . afterAll cleanup $ do
        context "with config file not found" -- invalidate config path
            $ beforeWith (\env -> return $ env { envConfigPath = "invalidPath" }) $
            context "with application ran"
                    $ beforeWith (startApplication >=> waitForApplicationDone) $
                it "cannot log" $ \(asyncResult, logEntries) -> do
                    first E.fromException asyncResult `shouldBe` Left (Just $ App.AppException
                        "Read config file: invalidPath: openFile: does not exist \
                        \(No such file or directory)")
                    logEntries `shouldSatisfy` null
        context "with invalid config file format"
            $ beforeWith (\env -> return $ env { envConfigPath = "test/fixtures/invalid.ini" }) $
            context "with application ran"
                    $ beforeWith (startApplication >=> waitForApplicationDone) $
                it "cannot log" $ \(asyncResult, logEntries) -> do
                    either (maybe "" App.exceptionMsg . E.fromException)
                           (const "")
                           asyncResult
                       `shouldStartWith` "Parse config file:"
                    logEntries `shouldSatisfy` null
        context "with invalid redis port" -- invalidate port number
             $ beforeWith (\env -> return $ env
                 { envConnectInfo = (envConnectInfo env) { R.connectPort = R.PortNumber 1 }}) $
            context "with application ran"
                    $ beforeWith (startApplication >=> waitForApplicationDone) $
                it "stopped logging after startup message" $ \(asyncResult, logEntries) -> do
                    either (maybe "" App.exceptionMsg . E.fromException)
                           (const "")
                           asyncResult
                       `shouldStartWith` "Initialize database: Network.Socket.connect:"
                    logEntries `shouldSatisfy` (not . null)
                    (TL.unpack . snd . last) logEntries `shouldStartWith` "Startup "
        context "with invalid redis host" -- invalidate host
             $ beforeWith (\env -> return $ env
                { envConnectInfo = (envConnectInfo env) { R.connectHost = "unknownHost" }}) $
            context "with application ran"
                    $ beforeWith (startApplication >=> waitForApplicationDone) $
                it "stopped logging after startup message" $ \(asyncResult, logEntries) -> do
                    first E.fromException asyncResult `shouldBe` Left (Just $ App.AppException
                        "Initialize database: Network.BSD.getHostByName: does not exist \
                        \(no such host entry)")
                    logEntries `shouldSatisfy` (not . null)
                    (TL.unpack . snd . last) logEntries `shouldStartWith` "Startup "
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
                                "Initial configuration"
                            (TL.unpack . snd . last) logEntries `shouldBe`
                                "Shutdown complete"
                            -- see http://man7.org/linux/man-pages/man7/signal.7.html
                            logEntries `shouldSatisfy` ((Log.Info, "Caught signal 10, ignore") `elem`)
                            -- note: we expect "U ..done" to follow termination signal because
                            -- this event is uninterruptable
                            dropWhile (/= (Log.Info, "Caught signal 2, terminate")) logEntries
                                `shouldSatisfy` ((Log.Info, "U ..done") `elem`)
  where
    waitForApplicationDone env = do
        -- wait for application to finish
        asyncResult <- E.try @E.SomeException
                     $ CA.wait (fromMaybe (error "Async not set") $ envAppAsync env)
        -- return log entries in chronological order
        logEntries <- maybe [] (reverse . List.resourceSink) <$> C.tryReadMVar (envLog env)
        return (asyncResult, logEntries)
    startApplication env = do
        -- reset resources, otherwise the effect init function would block undefinitely
        void $ C.tryTakeMVar $ envLog env
        void $ C.tryTakeMVar $ envRedis env
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
    redis <- C.newEmptyMVar
    log' <- C.newEmptyMVar
    return $ Env state log' redis filePath processHandle Nothing patchedConnectInfo
  where
    filePath = "test/fixtures/valid.ini"

-- cleanup context
cleanup :: Env -> IO ()
cleanup = Process.terminateProcess . envRedisProcessHandle -- send SIGTERM

