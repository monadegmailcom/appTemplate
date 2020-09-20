{- | The application implementation. -}
module App ( app ) where

import qualified Effect.CmdLine as CmdLine
import qualified Effect.Redis as Redis
import qualified Effect.Filesystem as Filesystem
import qualified Effect.Signal as Signal
import qualified Effect.Time as Time
import qualified Effect.Concurrent.Stream as Stream
import qualified Effect.Concurrent.STM as STM
import qualified Effect.Concurrent.Thread as Thread
import qualified Config
import qualified Log
import qualified Poll

import qualified Control.Exception.Safe as E
import           Control.Monad (void, when)
import           Data.Bifunctor (first)
import           Data.Maybe (isJust, fromJust)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Version as Version
import           Formatting ((%))
import qualified Formatting as F
import qualified Paths_appTemplate as Paths
import qualified Streamly as S
import qualified Streamly.Prelude as S
import qualified System.Posix.Signals as PS
import qualified Time.Units

-- | Application exception.
newtype AppException = AppException { exceptionMsg :: TL.Text }
    deriving (Show, Eq)
    deriving anyclass E.Exception

-- | The application runs through these run level phases.
data RunLevel
    = -- | Set on startup, services like logging, redis, etc will be
      --   started as soon as preconditional values are set (e.g.
      --   configuration for filelogging or redis connection).
      Events
      -- | Set by termination signal, no more asynchronous
      --   event processing will be started, wait for ongoing
      --   processing to finish. In this run level, logging is
      --   still operational.
    | Logging
      -- | Application process terminates
    | Stopped
    deriving (Eq, Ord)

{- | Function blocks until application shutdown.
     Only effects from the contraint list are allowed, no IO operations. -}
app :: ( E.MonadCatch m
       , Thread.ThreadM m
       , Stream.StreamM m
       , STM.STM m
       , CmdLine.CmdLineM m
       , Filesystem.FilesystemM m h
       , Redis.RedisM m c
       , Signal.SignalM m
       , Time.TimeM m)
    => m ()
app = do
    -- the initial run level
    runLevelR <- STM.newTVar' Events

    -- application wide state
    stateR <- STM.newTVar' 0

    -- the log handle relay determines the logging sink, the default stdout
    -- will be replaced when the config is read.
    logHandleR <-
            Filesystem.stdout
        >>= STM.newTVar'

    -- the log resource consists of a fifo queue for delay-free msg logging
    -- (a channel) and a minimal log level to optionally reduce logging
    -- noise, the default Log.Info will be replaced when the config is read.
    logRes <-
            Log.Resource
        <$> STM.newTChan'
        <*> STM.newTVar' Log.Info

    -- the commandline options relay provides access to the parameter
    -- passed to the application on the commandline
    cmdlOptR <- STM.newEmptyTMVar'

    -- the config relay provides access to the application configuration,
    -- will be read from a ini file
    configR <- STM.newEmptyTMVar'

    -- the redis connection relay
    redisConR <- STM.newEmptyTMVar'

    -- events to be processed in "Events" run level
    let processEvents =
            S.after (shutdownLogging logRes runLevelR)
          . S.foldWith Stream.parallel
          $ [ greet logRes
            , readCmdLineOptions logRes cmdlOptR
            , installSignalHandlers logRes runLevelR
            , readConfigFile logRes cmdlOptR configR
            , initLogging logRes configR logHandleR
            , processStateRequests logRes runLevelR stateR
            , processRedisRequests logRes runLevelR redisConR
            , reconnectToRedis logRes runLevelR redisConR configR
            ]
        -- logging events processed before "Stopped" run level
        processLogging = logging logRes runLevelR logHandleR

    -- run event processing in parallel, logging is processed single
    -- threaded, other events are processed multithreaded
    S.drain $ processLogging `Stream.parallel` processEvents
  where
    shutdownLogging logRes runLevelR =
          STM.writeTVar' runLevelR Stopped
          -- flush to wake up potentially blocking read on log channel
       >> Log.flush logRes
    untilShutdown runLevelR stream =
        let blockWhileRunning =
                 STM.atomically
               $ STM.readTVar runLevelR
             >>= (`when` STM.retry) . (== Events)
              >> return Nothing
        in S.map fromJust
         . S.takeWhile isJust
         $ Stream.parallel (S.yieldM blockWhileRunning)
                           (S.map Just stream)
    processStateRequests logRes runLevelR stateR =
        let produceStateRequest
                  = Thread.delay (Time.Units.sec 1)
                 >> STM.readTVar' stateR
                >>= Log.info logRes . F.format ("Request " % F.int)
        in Stream.mapM (const $ Poll.runState logRes stateR)
         . S.finally (Log.debug logRes "Stopped generating state requests")
         . untilShutdown runLevelR
         . Stream.repeatM
         $ produceStateRequest
    processRedisRequests logRes runLevelR redisConR =
        let produceRedisRequest =
                Thread.delay (Time.Units.sec 1.5)
             >> Log.info logRes "Request redis"
            str = "Exception while processing redis request, reset\
                  \ redis connection: "
            onE e = STM.tryTakeTMVar' redisConR -- reset redis connection
                 >> Log.warning logRes (F.format (str % F.shown) e)
            withTimeout action =
                 Stream.timeout (Time.Units.sec 1) action
             >>= maybe (E.throw $ AppException "Timeout") return
            process =
                 withTimeout
               $ STM.readTMVar' redisConR
             >>= Poll.runRedis logRes
        in Stream.mapM (const $ E.handleAny onE process)
         . S.finally (Log.debug logRes "Stopped generating redis requests")
         . untilShutdown runLevelR
         . Stream.repeatM
         $ produceRedisRequest
    reconnectToRedis logRes runLevelR redisConR configR =
        let connect config = E.handleAny handleRedisConnectError
               $ Log.info logRes "Connect to redis database"
              >> Redis.connect config
             >>= STM.putTMVar' redisConR
              >> Log.info logRes "Connection to redis database established"
            handleRedisConnectError (e :: E.SomeException) =
                  Log.warning logRes
                $ F.format ("Exception while connecting to redis: " % F.shown) e
            waitForConnectionReset =
                  STM.atomically
                $ STM.tryReadTMVar redisConR
              >>= (`when` STM.retry) . isJust
            getConfig =
                    waitForConnectionReset
                 >> Config.redisConnectInfo . Config.configRedis . fst
                <$> STM.readTMVar' configR
        in Stream.mapM connect
         . S.finally (Log.debug logRes
                                "Stopped checking redis connection")
         . untilShutdown runLevelR
         $ S.serial -- sanity delay between two connect attempts
              (S.yieldM getConfig)
              (Stream.repeatM $ Thread.delay (Time.Units.sec 1)
                             >> getConfig)
    greet logRes =
          S.yieldM
        . Log.info logRes
        $ F.format
            ("Startup version " % F.string)
            (Version.showVersion Paths.version)
    readCmdLineOptions logRes cmdlOptR =
          S.yieldM
        $ Log.info logRes "Parse command line"
       >> CmdLine.parseCommandLineOptions
      >>= STM.putTMVar' cmdlOptR
    installSignalHandlers logRes runLevelR =
        let handler signal = do
                Log.info logRes $ F.format ("Caught signal " % F.shown)
                                           signal
                if signal `elem` terminateSignals
                then Log.info logRes "Shutdown"
                  >> STM.writeTVar' runLevelR Logging
                else Log.info logRes
                   $ F.format ("Signal " % F.shown % " ignored") signal
        in S.yieldM
         . mapM_ (Signal.installHandler (handler . PS.siginfoSignal))
         $ PS.sigUSR1 : terminateSignals
    readConfigFile logRes cmdlOptR configR = S.yieldM $ do
        CmdLine.CommandLineOptions path <- STM.readTMVar' cmdlOptR
        Log.info logRes $ F.format ("Open config file '" % F.string % "'")
                                   path
        handle <- Filesystem.openFile path Filesystem.ReadMode
        Log.info logRes "Read config file"
        content <- Filesystem.hGetContents handle
        Log.info logRes "Parse config file"
        config <- either (E.throwM . AppException . TL.pack) return
                $ first show (TL.decodeUtf8' content)
              >>= Config.parseIniFile . TL.toStrict
        Log.info logRes
          $ F.format ("Configuration:\n" % F.stext) (snd config)
        STM.putTMVar' configR config
    initLogging logRes configR logHandleR = S.yieldM $ do
        Config.Log logDestination minLogLevel <-
               Config.configLog . fst
           <$> STM.readTMVar' configR
        logHandle <- case logDestination of
            Config.StdOut -> Filesystem.stdout
            Config.File fp -> do
                Log.info logRes $ F.format ("Open log file '" % F.string % "'") fp
                Filesystem.openFile fp Filesystem.AppendMode
        void . STM.atomically $
               STM.swapTVar (Log.resourceMinLevelRelay logRes) minLogLevel
            >> STM.swapTVar logHandleR logHandle
    logging logRes runLevelR logHandleR =
          let isLogging = \case
                Log.Flush -> (< Stopped) <$> STM.readTVar' runLevelR
                Log.Msg {} -> return True
              msgStream = S.takeWhileM isLogging
                        . Log.stream
                        . Log.resourceMsgChan
                        $ logRes
              handleStream = Stream.repeatM
                           . STM.readTVar'
                           $ logHandleR
          in S.zipWithM (flip Log.logger) msgStream handleStream

-- list of signals on which we want to terminate, this conforms to the linux defaults,
-- see http://man7.org/linux/man-pages/man7/signal.7.html
-- exception is SIGPIPE, which we want to ignore
terminateSignals :: [PS.Signal]
terminateSignals = [PS.sigHUP, PS.sigINT, PS.sigALRM, PS.sigTERM]

