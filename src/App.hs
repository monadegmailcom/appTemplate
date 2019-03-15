{- | Contains the entry point of the application. It is located in the library to be accessible by
     the test target.
-}
module App
    ( run
    ) where

import qualified Control.Concurrent as C
import qualified Control.Concurrent.Async as CA
import           Control.Exception.Safe (MonadMask, bracket_, finally)
import           Control.Monad (void, unless)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader (MonadReader, ask, asks, runReaderT)
import qualified Data.Text as T
import qualified Data.Version as Version
import           Environment (Environment(..))
import qualified Log
import qualified Paths_appTemplate as Paths
import qualified Signals
import qualified System.Posix.Signals as PS

{- | Run application, it blocks until it stops processing by sending a STOP signal or setting
     the shutdown variable in 'Environment'.
     The 'Environment' provided in a reader monad gives access to external services like logging
     and application state.
     Initialize by setting signal handlers and say hello in log.
     When finished initializing, set busy state to idle, even in the case of exception thrown at
     startup.
     Example usage, see "app/Main.hs"
-}
run :: (MonadIO m, MonadMask m, MonadReader Environment m) => m ()
run = flip finally setIdle $ do
    -- log hello message
    Log.info $ "Startup version " <> Version.showVersion Paths.version

    -- install signal handlers
    mapM_ installHandler Signals.terminateSignals

    -- set to idle when initialization is done
    setIdle

    -- optionally we may start some asynchronous event processing. The poll function
    -- terminates when a shutdown is requested, wait for this to happen. Note: when we
    -- omit this optional line, the blocking read call to `envShutdown` in the 
    -- following line is important.
    ask >>= liftIO . CA.async . runReaderT poll >>= liftIO . CA.wait

    -- block until shutdown is requested, e.g. by signal handler
    asks envShutdown >>= liftIO . C.readMVar

    -- log goodbye message
    Log.info ("Shutdown complete" :: T.Text)
  where
    installHandler signal = do
        env <- ask
        Signals.installHandler (getSignalHandler signal env) signal
    getSignalHandler = runReaderT . signalHandler
    setIdle = asks envBusy >>= void . liftIO . C.tryTakeMVar

-- only one IO action can run at a time when processed with `synchronize`
synchronize :: (MonadMask m, MonadIO m, MonadReader Environment m) => m a -> m a
synchronize = bracket_ claim release
  where
    claim = asks envBusy >>= liftIO . flip C.putMVar ()
    release = asks envBusy >>= liftIO . C.takeMVar

-- report the signal and initiate shutdown
signalHandler :: (MonadMask m, MonadIO m, MonadReader Environment m) => PS.Signal -> m ()
signalHandler signal = do
    Log.info $ "Caught signal " <> (T.pack . show $ signal) <> ", shutting down..."

    -- block if application is busy already, so we do not interrupt
    -- the application while doing important business
    asks envShutdown >>= synchronize . liftIO . flip C.putMVar ()

-- example async processing
poll :: (MonadIO m, MonadMask m, MonadReader Environment m) => m ()
poll = loopUnless isShuttingDown $ do
    synchronize $ Log.info ("poll" :: String) >> waitSome

    -- wait some time for signal handler to run
    waitSome
  where
    waitSome = liftIO . C.threadDelay $ 100000 -- 1/10 sec
    loopUnless predicate action = predicate >>= flip unless (action >> loopUnless predicate action)
    isShuttingDown = asks envShutdown >>= liftIO . fmap not . C.isEmptyMVar

