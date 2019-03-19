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
    Log.info $ "Startup version " <> Version.showVersion Paths.version

    -- optionally we may start some asynchronous event processing. The poll function
    -- terminates when a shutdown is requested, wait for this to happen. Note: when we
    -- omit this optional line, the blocking read call to `envShutdown` in the
    -- following line is important.
    ask >>= liftIO . CA.async . runReaderT (poll "A") >>= liftIO . CA.wait

    -- block until shutdown is requested (e.g. by signal handler) and no transactions are pending
    GracefulShutdown.waitFor

    -- log goodbye message
    Log.info ("Shutdown complete" :: T.Text)

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

-- example async processing
poll :: (MonadIO m, MonadMask m, MonadReader Environment m) => T.Text -> m ()
poll msg = loopUnless GracefulShutdown.isScheduled $ do
    GracefulShutdown.suspendWhile $ Log.info ("poll " <> msg) >> waitSome

    -- wait some time for signal handler to run
    waitSome
  where
    waitSome = liftIO . threadDelay $ 100000 -- 1/10 sec
    loopUnless predicate action = predicate >>= flip unless (action >> loopUnless predicate action)

