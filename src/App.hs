{- | Contains the entry point of the application. Address following requirements:

     - 'installSignalHandlers' to allow a graceful shutdown.
     - 'run' the application in an 'Environment' providing
         - external service like logging and database access
         - a 'guard' for graceful shutdown
     - 'run' is accessible both from application and test framework for unit testing.
     - 'run' starts any necessary event processing functions and blocks until shutdown
       requested by the 'Guard'
-}
module App
    ( installSignalHandlers
    , run
    ) where

import           Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as CA
import           Control.Exception.Safe (MonadMask)
import           Control.Monad (unless, void)
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Control.Monad.Reader (MonadReader, ask, runReaderT)
import qualified Data.Text as T
import qualified Data.Version as Version
import           Environment (Environment(..))
import qualified GracefulShutdown
import qualified Log
import qualified Paths_appTemplate as Paths
import qualified Signals
import qualified System.Posix.Signals as PS

{- | Run application, it blocks until it stops processing when receiving a terminate signal
     or requesting the shutdown by the 'Guard'.
     The 'Environment' provides in a reader monad access to external services like logging
     and application state.
     For example usage, see "Main.hs" or "AppSpec.hs"
-}
run :: (MonadIO m, MonadReader Environment m) => m ()
run = do
    -- log hello message
    Log.info $ "Startup version " <> (T.pack . Version.showVersion) Paths.version

    -- optionally we may start some asynchronous event processing. The poll function
    -- terminates when a shutdown is requested
    env <- ask
    mapM_ (liftIO . CA.async) [poll' "A" env, poll' "B" env]

    -- block until shutdown is requested (e.g. by signal handler) and no transactions are pending
    GracefulShutdown.wait

    -- log goodbye message
    Log.info "Shutdown complete"
  where
    poll' = runReaderT . poll

-- | Install signal handlers.
--installSignalHandlers :: (MonadIO m, MonadReader Environment m) => m ()
--installSignalHandlers = mapM_ installHandler Signals.terminateSignals
--  where
--    installHandler signal = do
--        env <- ask
--        Signals.installHandler (getSignalHandler signal env) signal
--    getSignalHandler = runReaderT . signalHandler
installSignalHandlers :: (MonadIO m, Log.HasLog m, GracefulShutdown.HasGuard m) => m ()
installSignalHandlers = mapM_ installHandler Signals.terminateSignals
  where
    installHandler signal = do
        env <- Environment undefined <$> Log.getLogFunction <*> GracefulShutdown.getGuard
        Signals.installHandler (getSignalHandler signal env) signal
    getSignalHandler = runReaderT . signalHandler

-- report the signal and initiate shutdown
signalHandler :: (MonadIO m, Log.HasLog m, GracefulShutdown.HasGuard m) => PS.Signal -> m ()
signalHandler signal = do
    Log.info $ "Caught signal " <> (T.pack . show $ signal) <> ", shutting down..."
    GracefulShutdown.request

-- example async processing, return when shutdown is requested
poll :: (MonadIO m, MonadMask m, MonadReader Environment m) => T.Text -> m ()
poll msg = loopUnless GracefulShutdown.isScheduled $ do
    void . GracefulShutdown.guard
        $ Log.info ("poll " <> msg) >> waitSome >> Log.info (msg <> " ..done")

    -- wait some time for signal handler to run
    waitSome
  where
    waitSome = liftIO . threadDelay $ 100000 -- 1/10 sec
    loopUnless exitCondition action =
        exitCondition >>= flip unless (action >> loopUnless exitCondition action)

