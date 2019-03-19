{- | Contains the entry point of the application. It is located in the library to be accessible by
     the test target.
-}
module App
    ( installSignalHandlers
    , run
    ) where

import           Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as CA
import           Control.Exception.Safe (MonadMask)
import           Control.Monad (unless)
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Control.Monad.Reader (MonadReader, ask, runReaderT)
import qualified Data.Text as T
import qualified Data.Version as Version
import           Environment (Environment)
import qualified GracefulShutdown
import qualified Log
import qualified Paths_appTemplate as Paths
import qualified Signals
import qualified System.Posix.Signals as PS

{- | Run application, it blocks until it stops processing by sending a STOP signal or setting
     the shutdown variable in 'Environment'.
     The 'Environment' provided in a reader monad gives access to external services like logging
     and application state.
     For example usage, see "app/Main.hs" or "test/AppSpec.hs"
-}
run :: (MonadIO m, MonadReader Environment m) => m ()
run = do
    -- log hello message
    Log.info $ "Startup version " <> (T.pack . Version.showVersion) Paths.version

    -- optionally we may start some asynchronous event processing. The poll function
    -- terminates when a shutdown is requested, wait for this to happen.
    env <- ask
    mapM_ (liftIO . CA.async) [poll' "A" env, poll' "B" env]

    -- block until shutdown is requested (e.g. by signal handler) and no transactions are pending
    GracefulShutdown.waitFor

    -- log goodbye message
    Log.info "Shutdown complete"
  where
    poll' = runReaderT . poll

-- | Install signal handlers.
installSignalHandlers :: (MonadIO m, MonadReader Environment m) => m ()
installSignalHandlers = mapM_ installHandler Signals.terminateSignals
  where
    installHandler signal = do
        env <- ask
        Signals.installHandler (getSignalHandler signal env) signal
    getSignalHandler = runReaderT . signalHandler

-- report the signal and initiate shutdown
signalHandler :: (MonadMask m, MonadIO m, MonadReader Environment m) => PS.Signal -> m ()
signalHandler signal = do
    Log.info $ "Caught signal " <> (T.pack . show $ signal) <> ", shutting down..."
    GracefulShutdown.request

-- example async processing, return when shutdown is requested
poll :: (MonadIO m, MonadMask m, MonadReader Environment m) => T.Text -> m ()
poll msg = loopUnless GracefulShutdown.isScheduled $ do
    GracefulShutdown.guard
        $ Log.info ("poll " <> msg) >> waitSome >> Log.info (msg <> " ..done")

    -- wait some time for signal handler to run
    waitSome
  where
    waitSome = liftIO . threadDelay $ 100000 -- 1/10 sec
    loopUnless exitCondition action =
        exitCondition >>= flip unless (action >> loopUnless exitCondition action)

