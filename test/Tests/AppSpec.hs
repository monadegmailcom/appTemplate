-- disable warning concerning Redis.PortID, I cannot do anything about it
{-# OPTIONS_GHC -Wno-deprecations #-}

module Tests.AppSpec  where
--module Tests.AppSpec (spec) where

import qualified App.Impl
import qualified Config
import qualified Control.Concurrent as C
import qualified Control.Concurrent.Async as CA
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as E
import           Control.Monad ((>=>))
import           Control.Monad.Reader (runReaderT)
import           Control.Monad.Trans.Control (control)
import qualified Data.ByteString.Char8 as BS
import           Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Database.Redis as Redis
import qualified Effect.CmdLine as CmdLine
import qualified Effect.CmdLine.Impl as CmdLine ()
import qualified Effect.Database.Impl as Database.Impl
import qualified Effect.Log as Log
import qualified Effect.State.Impl as State
import qualified GHC.IO.Handle as Handle
import qualified Mock.Environment
import qualified Network.Socket as Socket
import qualified System.Environment
import qualified System.Posix.Signals as PS
import qualified System.Process as Process
import           Test.Hspec
import qualified Time.Units

spec :: Spec
spec = context "App" $
        context "with valid config" $ beforeAll setup . afterAll cleanup $
--            context "with redis connect at startup failed" $
            context "with application started" $ beforeWith startApplication $
                context "with USR1 and INT signals sent" $ beforeWith
                    (\ctx -> mapM_ PS.raiseSignal [PS.sigUSR1, PS.sigINT] >> return ctx) $
                    context "with application shutdown" $ beforeWith waitForApplicationDone $
                        it "logs as expected" $ \logEntries -> do
                            -- see http://man7.org/linux/man-pages/man7/signal.7.html
                            logEntries `shouldSatisfy` ((Log.Info, "Caught signal 10, ignore") `elem`)
                            -- note: we expect "U ..done" to follow termination signal because
                            -- this event is uninterruptable
                            dropWhile (/= (Log.Info, "Caught signal 2, terminate")) logEntries
                                `shouldSatisfy` ((Log.Info, "U ..done") `elem`)
  where
    waitForApplicationDone (logSink, appAsync) = do
        -- wait for application to finish
        CA.wait appAsync `shouldThrow` isJust @E.SomeAsyncException . E.fromException
        -- return log entries in chronological order
        reverse <$> C.readMVar logSink
    startApplication ctx = do
        -- start application asynchronously
        appAsync <- flip runReaderT (ctxEnv ctx) $ do
            async <- control $ \runInIO -> CA.async $ runInIO App.Impl.runPollers
            App.Impl.installSignalHandlers . CA.asyncThreadId $ async

            return async
        return (ctxLogSink ctx, appAsync)

-- test context
data Context = Context { ctxEnv :: App.Impl.Env
                       , ctxLogSink :: C.MVar [(Log.Level,TL.Text)]
                       , ctxRedisProcessHandle :: C.MVar Process.ProcessHandle }

-- setup context
setup :: IO Context
setup = do
    port <- getFreePort
    config <- System.Environment.withArgs ["-c", fixturesDir <> configFile] $
                     CmdLine.parseCommandLineOptions
                 >>= T.readFile . CmdLine.cmdLineConfigFile
                 >>= either error (return . patchPort port . fst) . Config.parseIniFile
    let redisConfig = Config.configRedis config
    processHandle <- startRedis (Config.redisConnectInfo redisConfig) >>= C.newMVar
    redis <- do
        redisConnection <- Redis.checkedConnect . Config.redisConnectInfo $ redisConfig
        STM.newTVarIO $ Database.Impl.Redis redisConnection redisConfig
    logSink <- C.newMVar []
    state <- State.defaultState
    let env = App.Impl.Env (Mock.Environment.createLogFunction logSink) state redis
    return $ Context env logSink processHandle
  where
    configFile = "valid.ini"
    fixturesDir = "test/fixtures/"
    patchPort port config = let redis = Config.configRedis config
                                ci = (Config.redisConnectInfo redis)
                                         { Redis.connectPort = Redis.PortNumber port }
                            in config { Config.configRedis = Config.Redis ci }

-- cleanup context
cleanup :: Context -> IO ()
cleanup = C.takeMVar . ctxRedisProcessHandle
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

startRedis :: Redis.ConnectInfo -> IO Process.ProcessHandle
startRedis connectInfo = do
    -- start redis process on free port and redirect stdout to a pipe
    (mInHandle, mOutHandle, _, processHandle) <- Process.createProcess $
         process { Process.std_out = Process.CreatePipe, Process.std_in = Process.CreatePipe }
    do -- write redis config to stdin
        let inHandle = fromMaybe (error "no in handle") mInHandle
            configStr = case Redis.connectPort connectInfo of
                            Redis.PortNumber port -> "port " ++ show port ++ "\n"
                            _ -> error "no port defined"
                        ++ "bind " ++ Redis.connectHost connectInfo ++ "\n"
                        ++ maybe "" (("requirepass " ++) . BS.unpack) (Redis.connectAuth connectInfo)
        Handle.hPutStr inHandle configStr
        Handle.hClose inHandle -- the redis server starts when stdin closes
    do -- wait (with timeout) for redis server to be ready accepting connections
        let outHandle = fromMaybe (error "no out handle") mOutHandle
        Time.Units.timeout startupTimeout (outHandle `waitFor` "Ready to accept connections")
            >>= maybe (error "mongod not ready within timeout") return
    return processHandle
  where
--    process = Process.proc "redis-server" ["--port", show port]
    process = Process.proc "redis-server" ["-"]
    startupTimeout = Time.Units.sec 5

-- wait until trigger string is read from handle
waitFor :: Handle.Handle -> T.Text -> IO ()
waitFor handle str = snd . T.breakOn str <$> T.hGetLine handle >>= \case
    "" -> waitFor handle str -- substring not found, keep waiting
    _ -> return () -- substring found, exit loop
