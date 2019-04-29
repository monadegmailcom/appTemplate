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
import qualified Control.Exception as E
import           Control.Monad (forever)
import           Control.Monad.IO.Class (liftIO, MonadIO)
import qualified Data.Text as T
import qualified Data.Version as Version
import qualified Log
import           MultiReader ((<:>))
import qualified MultiReader as MR
import qualified Paths_appTemplate as Paths
import qualified System.Posix.Signals as PS

{- | Start some event processing functions and blocks until all finish (e.g. by
     receiving an async exception from signal handler). For example usage,
     see "Main.hs" or "AppSpec.hs" -}
run :: (MonadIO m, Config.HasConfig m, Log.HasLog m) => m ()
run = do
    -- log hello message with version info
    Log.info $ "Startup version " <> (T.pack . Version.showVersion) Paths.version
    -- log current (read-only) configuration, will not change until application restarts
    Config.getConfig >>= Log.info . ("Initial configuration " <>) . T.pack . show

    -- optionally we may start some asynchronous event processing in an appropriate
    -- environment. The poll functions terminate when an async exceptions is thrown to
    -- this thread. Notice: one of the poller is run masked uninterruptible, so it will
    -- delay termination until next iteration
    logFunction <- Log.getLogFunction
    let env = logFunction <:> MR.nil
        runPoll = MR.runReader env . poll
        pollers =
            [ E.uninterruptibleMask_ $ runPoll "U"
            , runPoll "A"
            , runPoll "B"]
    -- call pollers forever and log goodbye message when 'run' terminates
    -- when this thread receives as async exception all pollers are cancelled
    -- when either poller throws an exception all sybling poller are cancelled
    liftIO $ E.finally (CA.mapConcurrently_ forever pollers)
                       (MR.runReader env $ Log.info "Shutdown complete")

-- example async processing
poll :: (MonadIO m, Log.HasLog m) => T.Text -> m ()
poll msg = Log.info ("poll " <> msg) >> waitSome >> Log.info (msg <> " ..done")
  where
    waitSome = liftIO . C.threadDelay $ 1000000 -- 1 sec

-- | Install signal handlers. Terminate signals are transformed to async exception thrown to
-- given thread.
installSignalHandlers :: (MonadIO m, Log.HasLog m) => C.ThreadId -> m ()
installSignalHandlers threadId = do
    logFunction <- Log.getLogFunction
    let env = logFunction <:> MR.nil
        installHandler signalHandler signal =
            PS.installHandler signal (runSignalHandler env signalHandler) Nothing
    liftIO $    installHandler usr1SignalHandler PS.sigUSR1
             >> mapM_ (installHandler termSignalHandler) terminateSignals
  where
    -- transform to IO monad
    runSignalHandler env signalHandler = PS.CatchInfo $ MR.runReader env . signalHandler
    usr1SignalHandler signalInfo = Log.info $
        "Caught USR1 signal " <> (T.pack . show . PS.siginfoSignal $ signalInfo)
    termSignalHandler signalInfo = do
        Log.info $ "Caught terminate signal " <> (T.pack . show . PS.siginfoSignal $ signalInfo)
        liftIO $ E.throwTo threadId E.UserInterrupt
    -- list of signals on which we want to terminate, this conforms to the unix defaults,
    -- see http://man7.org/linux/man-pages/man7/signal.7.html
    terminateSignals = [PS.sigHUP, PS.sigINT, PS.sigPIPE, PS.sigALRM, PS.sigTERM]

