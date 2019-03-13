{- | Contains the entry point of the application. It is located in the library to be accessible by
     the test target.
-}
module App
    ( run
    ) where

import qualified Control.Concurrent as C
import           Control.Concurrent.Async (async)
import           Control.Exception.Safe (MonadMask, bracket_, finally)
import           Control.Monad (void, unless)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader (MonadReader, ask, asks, runReaderT)
import qualified Data.Text as T
import qualified Data.Version as Version
import           Environment (Environment(..))
import qualified Log
import qualified Paths_appTemplate as Paths
import           Settings (Settings(..))
import qualified Signals
import qualified System.Posix.Signals as PS

{- | Run application, it blocks until it stops processing by sending a STOP signal or setting
     the shutdown variable in 'Settings'.
     The 'Environment' provided in a reader monad gives access to external services like logging
     and application state in 'Settings'.
     Initialize by setting signal handlers and say hello in log.
     When finished initializing, set busy state to idle, even in the case of exception thrown at
     startup.
     Example usage, see "app/Main.hs"
-}
run :: (MonadIO m, MonadMask m, MonadReader Environment m) => m ()
run = flip finally setIdle $ do
    -- log hello message
    Log.info $ "Startup version " <> Version.showVersion Paths.version

    -- get environment
    env <- ask

    -- install signal handlers
    mapM_ (Signals.installHandler (getSignalHandler env)) Signals.terminateSignals

    -- set to idle when initialization is done
    setIdle

    -- optionally we may start some asynchronous event processing
    void . liftIO . async $ runReaderT poll env

    -- block until shutdown is requested, e.g. by signal handler
    liftIO . C.readMVar . settingsShutdown $ envSettings env

    -- log goodbye message
    Log.info ("Shutdown complete" :: T.Text)
  where
    getSignalHandler env = flip runReaderT env . signalHandler
    setIdle = unsetFlag settingsBusy

signalHandler :: (MonadIO m, MonadReader Environment m) => PS.SignalInfo -> m ()
signalHandler signalInfo = do
    Log.info $ "Caught signal "
            <> (T.pack . show . PS.siginfoSignal) signalInfo
            <> ", shutting down..."

    -- block if application is busy already, so we do not interrupt
    -- the application while doing important business.
    setFlag settingsBusy

    -- signal shutdown state
    setFlag settingsShutdown

    -- set to idle state
    unsetFlag settingsBusy

-- blocks if flag is already set
setFlag :: (MonadIO m, MonadReader Environment m) => (Settings -> C.MVar ()) -> m ()
setFlag flag = asks (flag . envSettings) >>= liftIO . flip C.putMVar ()

-- does not block
unsetFlag :: (MonadIO m, MonadReader Environment m) => (Settings -> C.MVar ()) -> m ()
unsetFlag flag = asks (flag . envSettings) >>= void . liftIO . C.tryTakeMVar

-- example async processing
poll :: (MonadIO m, MonadMask m, MonadReader Environment m) => m ()
poll = loopUnless isShuttingDown $ do
    bracket_
        (setFlag settingsBusy)
        (unsetFlag settingsBusy)
        (Log.info ("poll" :: String) >> (liftIO . C.threadDelay) 100000)

    -- wait some time for signal handler to run
    liftIO . C.threadDelay $ 100000
  where
    loopUnless predicate action = predicate >>= flip unless (action >> loopUnless predicate action)
    isShuttingDown = asks (settingsShutdown . envSettings) >>= liftIO . fmap not . C.isEmptyMVar

