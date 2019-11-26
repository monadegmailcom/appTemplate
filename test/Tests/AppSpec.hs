-- disable warning concerning Redis.PortID, I cannot do anything about it
{-# OPTIONS_GHC -Wno-deprecations #-}

module Tests.AppSpec  where

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

import qualified Control.Concurrent as C
import qualified Control.Concurrent.Async as CA
import qualified Control.Exception as E
import           Control.Monad ((>=>), void)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (asks, runReaderT, ReaderT)
import           Data.Bifunctor (first)
import qualified Data.ByteString.Char8 as BS
import           Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as T
import qualified Database.Redis as Redis
import qualified GHC.IO.Handle as Handle
import qualified Network.Socket as Socket
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
               , envRedisProcessHandle :: C.MVar Process.ProcessHandle
               , envAppAsync :: Maybe (CA.Async ())
               , envPortNumber :: Socket.PortNumber
               , envHost :: Maybe String
               }

type App = ReaderT Env IO

instance List.HasResource App where getResource = asks envLog
instance State.HasResource App where getResource = asks envState
instance Redis.HasResource App where getResource = asks envRedis

-- overwrite database redis initialization with port patched by a dynamically requested free port
instance {-# OVERLAPS #-} Database.InitM App where
    init config = do
        -- patch redis connect info with port and host
        portNumber <- asks envPortNumber
        mHost <- asks envHost
        let patchedConfig = config
                { Config.redisConnectInfo = (Config.redisConnectInfo config)
                    { Redis.connectPort = Redis.PortNumber portNumber
                    , Redis.connectHost = fromMaybe (Redis.connectHost . Config.redisConnectInfo
                                                        $ config) mHost
                    }}

        connection <- liftIO $ Redis.checkedConnect (Config.redisConnectInfo patchedConfig)
        asks envRedis >>= liftIO . (`C.putMVar` Redis.Resource connection patchedConfig)

spec :: Spec
spec = context "App" $
    context "with environment set up" $ beforeAll setup . afterAll cleanup $ do
        context "with config file not found" -- invalidate config path
            $ beforeWith (\env -> return $ env { envConfigPath = "invalidPath" }) $
            context "with application started" $ beforeWith startApplication $
                context "with application finished" $ beforeWith waitForApplicationDone $
                    it "cannot log" $ \(asyncResult, logEntries) -> do
                        first E.fromException asyncResult `shouldBe` Left (Just $ App.AppException
                            "Read config file: invalidPath: openFile: does not exist \
                            \(No such file or directory)")
                        logEntries `shouldSatisfy` null
        context "with invalid config file format"
            $ beforeWith (\env -> return $ env { envConfigPath = "test/fixtures/invalid.ini" }) $
            context "with application started" $ beforeWith startApplication $
                context "with application finished" $ beforeWith waitForApplicationDone $
                    it "cannot log" $ \(asyncResult, logEntries) -> do
                        first E.fromException asyncResult `shouldBe` Left (Just $ App.AppException
                            "Parse config file: Couldn't find key: Log.level")
                        either (maybe "" (\(App.AppException msg) -> msg) . E.fromException)
                               (const "")
                               asyncResult
                           `shouldStartWith` "Parse config file:"
                        logEntries `shouldSatisfy` null
        context "with invalid redis port" -- invalidate port number
             $ beforeWith (\env -> return $ env { envPortNumber = envPortNumber env + 1 }) $
            context "with application started" $ beforeWith startApplication $
                context "with application finished" $ beforeWith waitForApplicationDone $
                    it "stopped logging after startup message" $ \(asyncResult, logEntries) -> do
                        first E.fromException asyncResult `shouldBe` Left (Just $ App.AppException
                            "Initialize database: Network.Socket.connect: <socket: 15>: \
                            \does not exist (Connection refused)")
                        logEntries `shouldSatisfy` (not . null)
                        (TL.unpack . snd . last) logEntries `shouldStartWith` "Startup "
        context "with invalid redis host" -- invalidate host
             $ beforeWith (\env -> return $ env { envHost = Just "unknownHost" }) $
            context "with application started" $ beforeWith startApplication $
                context "with application finished" $ beforeWith waitForApplicationDone $
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
    port <- getFreePort
    processHandle <- do
        config <- System.Environment.withArgs ["-c", filePath] $
                         CmdLine.parseCommandLineOptions
                     >>= T.readFile . CmdLine.cmdLineConfigFile
                     >>= either error (return . fst) . Config.parseIniFile
        -- patch redis connect info with free port
        let redisConnectInfo = (Config.redisConnectInfo . Config.configRedis $ config)
                { Redis.connectPort = Redis.PortNumber port }
        startRedisServer redisConnectInfo >>= C.newMVar
    state <- State.defaultResource
    redis <- C.newEmptyMVar
    log' <- C.newEmptyMVar
    return $ Env state log' redis filePath processHandle Nothing port Nothing
  where
    filePath = "test/fixtures/valid.ini"

-- cleanup context
cleanup :: Env -> IO ()
cleanup = C.takeMVar . envRedisProcessHandle
      >=> Process.terminateProcess -- send SIGTERM

-- ask os for free socket
getFreePort :: IO Socket.PortNumber
getFreePort = E.bracket (Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol)
                        Socket.close $ \sock -> do
    Socket.bind sock address
    Socket.getSocketName sock >>= \case
        Socket.SockAddrInet port _ -> return port
        _ -> error "Unable to get free socket port"
  where
    address = Socket.SockAddrInet 0 $ Socket.tupleToHostAddress (127, 0, 0, 1)

startRedisServer :: Redis.ConnectInfo -> IO Process.ProcessHandle
startRedisServer connectInfo = do
    -- start redis process on free port and redirect stdout to a pipe
    (mInHandle, mOutHandle, _, processHandle) <- Process.createProcess $
         process { Process.std_out = Process.CreatePipe, Process.std_in = Process.CreatePipe }
    do -- write redis config to stdin
        let inHandle = fromMaybe (error "no in handle") mInHandle
            configStr = case Redis.connectPort connectInfo of
                            Redis.PortNumber port -> "port " ++ show port ++ "\n"
                            _ -> error "no port defined"
                        ++ maybe "" (("requirepass " ++) . BS.unpack) (Redis.connectAuth connectInfo)
        Handle.hPutStr inHandle configStr
        Handle.hClose inHandle -- the redis server starts when stdin closes
    do -- wait (with timeout) for redis server to be ready accepting connections
        let outHandle = fromMaybe (error "no out handle") mOutHandle
        Time.Units.timeout startupTimeout (outHandle `waitFor` "Ready to accept connections")
            >>= maybe (error "mongod not ready within timeout") return
    return processHandle
  where
    process = Process.proc "redis-server" ["-"]
    startupTimeout = Time.Units.sec 5

-- wait until trigger string is read from handle
waitFor :: Handle.Handle -> T.Text -> IO ()
waitFor handle str = snd . T.breakOn str <$> T.hGetLine handle >>= \case
    "" -> waitFor handle str -- substring not found, keep waiting
    _ -> return () -- substring found, exit loop
