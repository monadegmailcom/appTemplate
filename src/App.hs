{- | Contains the application's entry point. Address following requirements:

     - 'installSignalHandlers' to allow a graceful shutdown.
     - 'run' the application arguments providing access to
         - logging
         - configuration
         - additional resources like database or networking
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
import qualified Data.Text.Lazy as TL
import qualified Data.Version as Version
import           Formatting ((%))
import qualified Formatting as F
import qualified Log
import qualified Paths_appTemplate as Paths
import qualified State
import qualified System.Posix.Signals as PS

{- | Start some event processing functions and blocks until all finish (e.g. by
     receiving an async exception from signal handler). Note: log function, configuration
     and state are build externally to make these mockable. For example usage,
     see "Main.hs" or "AppSpec.hs" -}
run :: Log.Function -> Config.Config -> State.State -> IO ()
run logger _ state = do
    -- log hello message with version info
    logInfo $ F.format ("Startup version " % F.string) $ Version.showVersion Paths.version

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
    logInfo = logger Log.Info

-- example async processing
poll :: Log.Function -> State.State -> TL.Text -> IO ()
poll logger state msg =
        STM.atomically (State.incCount state)
    >>= logInfo . F.format ("poll " % F.text % " " % F.int) msg
     >> C.threadDelay 1000000 -- wait 1 sec
     >> logInfo (msg <> " ..done")
  where
    logInfo = logger Log.Info

-- | Install signal handlers. Terminate signals are transformed to async exception thrown to
-- given thread.
installSignalHandlers :: Log.Function -> C.ThreadId -> IO ()
installSignalHandlers logger threadId =
       installHandler usr1SignalHandler PS.sigUSR1
    >> mapM_ (installHandler termSignalHandler) terminateSignals
  where
    installHandler handler signal = PS.installHandler signal (PS.CatchInfo handler) Nothing
    logInfo = logger Log.Info
    toStr = F.format $ "Caught signal " % F.shown % ", " % F.text
    usr1SignalHandler signalInfo = logInfo $ toStr (PS.siginfoSignal signalInfo) "ignore"
    termSignalHandler signalInfo = do
        logInfo $ toStr (PS.siginfoSignal signalInfo) "terminate"
        E.throwTo threadId E.UserInterrupt
    -- list of signals on which we want to terminate, this conforms to the linux defaults,
    -- see http://man7.org/linux/man-pages/man7/signal.7.html
    -- exception is SIGPIPE, which we do want to ignore
    terminateSignals = [PS.sigHUP, PS.sigINT, PS.sigALRM, PS.sigTERM]

