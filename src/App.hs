{- | The application implementation.
     Note: the application implementation "app" is running
     in a monad restricted to use only effects from the contraint list, no IO operations
     other than those provided by effect constraints are allowed. -}
module App ( AppException(..)
           , app
           ) where

import qualified Config
import qualified Effect.CmdLine as CmdLine
import qualified Effect.Database as Database
import qualified Effect.Database.Init as Database
import qualified Effect.Filesystem as Filesystem
import qualified Effect.Log as Log
import qualified Effect.Log.Init as Log
import qualified Effect.Signal as Signal
import qualified Effect.State as State
import qualified Effect.Thread as Thread
import qualified Poll

import qualified Control.Concurrent as C
import qualified Control.Exception as E (AsyncException( UserInterrupt))
import qualified Control.Exception.Safe as E
import           Control.Monad (forever, void)
import qualified Data.Text.Lazy as TL
import qualified Data.Version as Version
import           Formatting ((%))
import qualified Formatting as F
import qualified Paths_appTemplate as Paths
import qualified System.Posix.Signals as PS

-- use for exception string annotation
newtype AppException = AppException String deriving (Show, Eq, E.Exception)

-- to annotate exceptions thrown by effect implementations with a semantic context
annotate :: (E.MonadCatch m) => String -> m a -> m a
annotate msg action = E.catchAny action (\e -> E.throw . AppException $ msg <> ": " <> show e)

{- | Implementation of application logic. Function blocks until application shutdown. -}
app :: ( E.MonadMask m
       , Thread.ThreadM m
       , CmdLine.CmdLineM m
       , Filesystem.FilesystemM m
       , Log.LogM m
       , Log.InitM m
       , Database.DatabaseM m
       , Database.InitM m
       , State.StateM m
       , Signal.SignalM m)
    => m ()
app = do
    annotate "Install signal handlers" installSignalHandlers
    -- parse configuration and initialize logging
    Config.Config {Config.configRedis} <- do
        (config, redactedConfigStr) <-
                    CmdLine.parseCommandLineOptions
                >>= annotate "Read config file" . Filesystem.readFile . CmdLine.cmdLineConfigFile
                >>= either (E.throwM . AppException . ("Parse config file: " <>))
                           return . Config.parseIniFile . TL.toStrict
        let Config.Log logDestination logLevel = Config.configLog config
        annotate "Init logging" $ Log.init logLevel logDestination
        -- log redacted configuration
        Log.info . F.format ("Initial configuration\n" % F.stext) $ redactedConfigStr
        return config
    Log.info . F.format ("Startup version " % F.string) $ Version.showVersion Paths.version
    annotate "Initialize database" $ Database.init configRedis
    annotate "Check database connection" $ void $ Database.getByKey "test"
    Log.info "Application initialized"
    -- call pollers forever and log goodbye message finally
    E.finally runPollers $ Log.info "Shutdown complete"

{- Start poller asynchronously. Poller will terminate on asynchronous exceptions.
   If the current thread receives an async exception (e.g. from the signal handlers)
   all pollers are cancelled.
   If any poller throws an exception all sibling pollers are cancelled.
   Note: one of the poller is masked uninterruptable, it will
   delay termination until next iteration in `forever`. -}
runPollers :: ( E.MonadMask m
              , Log.LogM m
              , Database.DatabaseM m
              , State.StateM m
              , Thread.ThreadM m)
           => m ()
runPollers = Thread.mapConcurrently forever
     [ E.uninterruptibleMask_ $ Poll.pollState "U"
     , Poll.pollState "S"
     , Poll.pollRedis "R1"
     , Poll.pollRedis "R2"
     ]

{- Install signal handlers. Terminate signals are transformed to async exception thrown to
   current thread. -}
installSignalHandlers :: (Log.LogM m, Signal.SignalM m, Thread.ThreadM m) => m ()
installSignalHandlers = do
    Signal.installHandler usr1SignalHandler PS.sigUSR1
    threadId <- Thread.myThreadId
    mapM_ (Signal.installHandler $ termSignalHandler threadId) terminateSignals
  where
    -- list of signals on which we want to terminate, this conforms to the linux defaults,
    -- see http://man7.org/linux/man-pages/man7/signal.7.html
    -- exception is SIGPIPE, which we want to ignore
    terminateSignals = [PS.sigHUP, PS.sigINT, PS.sigALRM, PS.sigTERM]

toStr :: TL.Text -> PS.SignalInfo -> TL.Text
toStr msg signalInfo =
    let signal = PS.siginfoSignal signalInfo
    in F.format ("Caught signal " % F.shown % ", " % F.text) signal msg

termSignalHandler :: (Log.LogM m, Thread.ThreadM m) => C.ThreadId -> PS.SignalInfo -> m ()
termSignalHandler threadId signalInfo = do
    Log.info $ toStr "terminate" signalInfo
    Thread.throwTo threadId E.UserInterrupt

usr1SignalHandler :: Log.LogM m => PS.SignalInfo -> m ()
usr1SignalHandler = Log.info . toStr "ignore"
