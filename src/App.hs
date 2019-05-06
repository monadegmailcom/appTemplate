{- | Contains the application's entry point. Address following requirements:

     - 'installSignalHandlers' to allow a graceful shutdown.
     - 'run' the application in an 'Environment' providing
         - access to configuration
         - logging
         - you may add additional resources like database access
     - 'run' is accessible both from application and test framework for unit testing.
-}
module App
    ( installSignalHandlers
    , run
    ) where

import qualified Config
import qualified Control.Concurrent as C
import qualified Control.Concurrent.Async as CA
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as E
import           Control.Monad (forever)
import qualified Data.Text as T
import qualified Data.Version as Version
import qualified Log
import qualified Paths_appTemplate as Paths
import qualified State
import qualified System.Posix.Signals as PS

{- | Start some event processing functions and blocks until all finish (e.g. by
     receiving an async exception from signal handler). For example usage,
     see "Main.hs" or "AppSpec.hs" -}
run :: Log.Function -> Config.Config -> State.State -> IO ()
run logger config state = do
    -- log hello message with version info
    logInfo $ "Startup version " <> (T.pack . Version.showVersion) Paths.version
    -- log current (read-only) configuration, will not change until application restarts
    logInfo . ("Initial configuration " <>) . T.pack . show $ config

    -- optionally we may start some asynchronous event processing in an appropriate
    -- environment. The poll functions terminate when an async exceptions is thrown to
    -- this thread. Notice: one of the poller is run masked uninterruptible, so it will
    -- delay termination until next iteration
    let runPoll = poll logger state
        pollers = [ E.uninterruptibleMask_ $ runPoll "U"
                  , runPoll "A"
                  , runPoll "B"]
    -- call pollers forever and log goodbye message finally
    -- when this thread receives as async exception all pollers are cancelled
    -- when either poller throws an exception all sibling pollers are cancelled
    E.finally (CA.mapConcurrently_ forever pollers)
              (logInfo "Shutdown complete")
  where
    logInfo = Log.info logger

-- example async processing
poll :: Log.Function -> State.State -> T.Text -> IO ()
poll logger state msg =
        STM.atomically (State.incCount state)
    >>= logInfo . ("poll " <>) . (msg <>) . (" " <>) . T.pack . show
    >>  waitSome
    >>  logInfo (msg <> " ..done")
  where
    logInfo = Log.info logger
    waitSome = C.threadDelay 1000000 -- 1 sec

-- | Install signal handlers. Terminate signals are transformed to async exception thrown to
-- given thread.
installSignalHandlers :: Log.Function -> C.ThreadId -> IO ()
installSignalHandlers logger threadId =
       installHandler usr1SignalHandler PS.sigUSR1
    >> mapM_ (installHandler termSignalHandler) terminateSignals
  where
    installHandler handler signal = PS.installHandler signal (PS.CatchInfo handler) Nothing
    logInfo = Log.info logger
    usr1SignalHandler signalInfo = logInfo $
        "Caught USR1 signal " <> (T.pack . show . PS.siginfoSignal $ signalInfo)
    termSignalHandler signalInfo = do
        logInfo $ "Caught terminate signal " <> (T.pack . show . PS.siginfoSignal $ signalInfo)
        E.throwTo threadId E.UserInterrupt
    -- list of signals on which we want to terminate, this conforms to the unix defaults,
    -- see http://man7.org/linux/man-pages/man7/signal.7.html
    terminateSignals = [PS.sigHUP, PS.sigINT, PS.sigPIPE, PS.sigALRM, PS.sigTERM]

