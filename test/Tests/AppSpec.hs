-- disable warning concerning Redis.PortID, I cannot do anything about it
{-# OPTIONS_GHC -Wno-deprecations #-}

module Tests.AppSpec where

import qualified App
import qualified Config
import           Effect.CmdLine.Impl ()
import           Effect.Redis.Impl ()
import qualified Effect.Filesystem.Memory as Filesystem
import qualified Effect.Signal as Signal
import           Effect.Signal.Impl ()
import           Effect.Time.Impl ()
import           Effect.Concurrent.Thread.Impl ()
import           Effect.Concurrent.STM.Impl ()
import           Effect.Concurrent.Stream.Impl ()
import qualified Helper.Redis
import qualified Log

import qualified Control.Concurrent.Async as CA
import qualified Control.Concurrent as C
import qualified Control.Concurrent.STM.TMVar as C
import           Control.Monad ((>=>))
import           Control.Monad.Reader (asks, runReaderT, ReaderT)
import           Data.Either (isLeft, isRight)
import qualified Data.Ini as Ini
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.Map as Map
import           Data.Maybe (isJust)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text as T
import qualified Database.Redis as R
import qualified System.Environment
import qualified System.Posix.Signals as PS
import qualified System.Process as Process
import           Test.Hspec
import qualified Time.Units

-- test context
data Context = Context { contextRedisProcessHandle :: Process.ProcessHandle
                       , contextConfigFileContent :: TL.Text
                       , contextConfigFilePath :: FilePath
                       }

data Env = Env { envLogChan :: C.TMVar (Maybe Log.Msg)
               , envSignalChan :: C.TMVar (Maybe Signal.SignalInfo)
               , envFs :: C.MVar (Map.Map FilePath [BL.ByteString])
               }

type App = ReaderT Env IO

instance Filesystem.HasResource App where getResource = asks envFs
instance R.MonadRedis App where liftRedis = R.liftRedis
instance R.RedisCtx App (Either R.Reply) where returnDecode = R.returnDecode

getLogEntries :: Env -> IO [TL.Text]
getLogEntries = fmap (reverse . map TL.decodeUtf8 . concat . Map.elems) . C.readMVar . envFs

patchConfigFileContent' :: (Ini.Ini -> Ini.Ini) -> TL.Text -> IO TL.Text
patchConfigFileContent' patch str =
    either error (return . TL.fromStrict . Ini.printIni . patch) eIni
  where
    eIni = Ini.parseIni . TL.toStrict $ str

patchConfigFileContent :: (Ini.Ini -> Ini.Ini) -> Context -> IO Context
patchConfigFileContent patch ctx
    = patchConfigFileContent' patch (contextConfigFileContent ctx)
  >>= \str -> return $ ctx { contextConfigFileContent = str }

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
            $ beforeWith (\ctx -> return $ ctx { contextConfigFilePath = "invalidPath" }) $
            context "with application ran"
                    $ beforeWith (startApplication >=> waitForApplicationDone) $
                it "logs as expected" $ \(env, e) -> do
                    e `shouldSatisfy` isLeft
                    logEntries <- getLogEntries env
                    logEntries `shouldSatisfy` contain "Read config file"
        context "with invalid config file format"
            $ beforeWith (\ctx -> return $ ctx { contextConfigFileContent = "bogus" }) $
            context "with application ran"
                    $ beforeWith (startApplication >=> waitForApplicationDone) $
                it "logs as expected" $ \(env, e) -> do
                    e `shouldSatisfy` isLeft
                    logEntries <- getLogEntries env
                    logEntries `shouldSatisfy` contain "Parse config file"
        context "with invalid redis port" -- invalidate port number
             $ beforeWith (patchConfigFileContent $ patchPort "1") $
            context "with application started" $ beforeWith startApplication $
                context "with INT signals sent" $ beforeWith
                    (\env -> PS.raiseSignal PS.sigINT >> return env) $
                    context "with application shutdown" $ beforeWith waitForApplicationDone $
                        it "logs redis error message" $ \(env, e) -> do
                            e `shouldSatisfy` isRight
                            logEntries <- getLogEntries env
                            logEntries `shouldSatisfy` contain "Network.Socket.connect:"
        context "with invalid redis host" -- invalidate host
             $ beforeWith (patchConfigFileContent (patchHost "redis://unknownHost" )) $
            context "with application started" $ beforeWith startApplication $
                context "with INT signals sent" $ beforeWith
                    (\env -> PS.raiseSignal PS.sigINT >> return env) $
                    context "with application shutdown" $ beforeWith waitForApplicationDone $
                    it "logs as expected" $ \(env, e) -> do
                        e `shouldSatisfy` isRight
                        logEntries <- getLogEntries env
                        logEntries `shouldSatisfy` contain "Name or service not known"
        context "with valid config" $
            context "with application started" $ beforeWith startApplication $
                context "with USR1 and INT signals sent" $ beforeWith
                    (\env -> mapM_ PS.raiseSignal [PS.sigUSR1, PS.sigINT] >> return env) $
                    context "with application shutdown" $ beforeWith waitForApplicationDone $
                        it "logs as expected" $ \(env, e) -> do
                            e `shouldSatisfy` isRight
                            logEntries <- getLogEntries env
                            logEntries `shouldSatisfy` contain "Startup version"
                            logEntries `shouldSatisfy` contain "Shutdown"
                            logEntries `shouldSatisfy` contain "Caught signal 2"
                            logEntries `shouldSatisfy` contain "Caught signal 10"
  where
    contain str = isJust . List.find (str `TL.isInfixOf`)
    waitForApplicationDone (appAsync, env) = (env, ) <$> CA.waitCatch appAsync

    startApplication ctx = do
        logChan <- C.newEmptyTMVarIO
        signalChan <- C.newEmptyTMVarIO
        fs <- C.newMVar
            . Map.singleton (contextConfigFilePath ctx)
            . reverse . map TL.encodeUtf8 . TL.splitOn "\n" . contextConfigFileContent
            $ ctx

        let env = Env logChan signalChan fs
        -- start application asynchronously
        appAsync <- CA.async $ System.Environment.withArgs ["-c", filePath] $ runReaderT App.app env
        -- wait some time for the app to start properly and the signalhandlers are installed
        -- TODO: watch log to decide when ready instead of waiting
        Time.Units.threadDelay $ Time.Units.sec 0.1
        return (appAsync, env)

filePath :: FilePath
filePath = "test/fixtures/valid.ini"

-- setup context
setup :: IO Context
setup = do
    port <- Helper.Redis.getFreePort "localhost"
    configFileContent <- TL.readFile filePath
                     >>= patchConfigFileContent' (patchPort . T.pack . show $ port)
    connectInfo <- either error (return . Config.redisConnectInfo . Config.configRedis . fst)
                 . Config.parseIniFile . TL.toStrict
                 $ configFileContent
    processHandle <- Helper.Redis.startServer connectInfo
    return $ Context processHandle configFileContent filePath

-- cleanup context
cleanup :: Context -> IO ()
cleanup = Process.terminateProcess . contextRedisProcessHandle -- send SIGTERM
