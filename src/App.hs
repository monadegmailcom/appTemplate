{- | Contains the entry point of the application. It is located in the library to be accessible by
     the test target.
-}
module App
    ( run
    ) where

import qualified Control.Concurrent as C
import Control.Concurrent.Async (async)
import Control.Exception.Safe (MonadMask, bracket_, finally)
import Control.Monad (void, forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask, asks, runReaderT)

import qualified Data.Text as T
import qualified Data.Version as Version

import qualified Log

import qualified Paths_appTemplate as Paths

import Settings (Settings(..))

import qualified Signals

import qualified System.Posix.Signals as PS

{- | Run application, it blocks until it stops processing, e.g. when it receives a STOP signal.
     Pass settings for global variables and external environment.
     Set busy state to idle, even in the case of exception thrown at startup.
     Example usage: `Environment.create config >>= Settings.create >>= runReaderT run`
-}
run :: (MonadIO m, MonadMask m, MonadReader Settings m) => m ()
run = flip finally setIdle $ do
    -- log hello message
    Log.info $ "Startup version " <> Version.showVersion Paths.version

    -- install signal handlers
    handler <- asks $ \settings -> flip runReaderT settings . signalHandler
    mapM_ (Signals.installHandler handler) Signals.terminateSignals

    -- set to idle when initialization is done
    setIdle

    -- optionally we may start some asynchronous event processing
    ask >>= void . liftIO . async . runReaderT poll

    -- block until shutdown is requested, e.g. by signal handler
    asks settingsShutdown >>= liftIO . C.readMVar

    Log.info ("Shutdown complete" :: T.Text)
  where
    setIdle = unsetFlag settingsBusy

signalHandler :: (MonadIO m, MonadReader Settings m) => PS.SignalInfo -> m ()
signalHandler signalInfo = do
    Log.info $ "Caught signal "
            <> (T.pack . show . PS.siginfoSignal) signalInfo
            <> ", shutting down..."

    -- block if application is busy already, so we do not interrupt
    -- the application doing important business
    setFlag settingsBusy

    -- because the application is now in busy state, no more processing, which respects
    -- the busy flag, will take place

    -- signal shutdown state
    setFlag settingsShutdown

-- blocks if flag is already set
setFlag :: (MonadIO m, MonadReader Settings m) => (Settings -> C.MVar ()) -> m ()
setFlag flag = asks flag >>= liftIO . flip C.putMVar ()

-- does not block
unsetFlag :: (MonadIO m, MonadReader Settings m) => (Settings -> C.MVar ()) -> m ()
unsetFlag flag = asks flag >>= void . liftIO . C.tryTakeMVar

poll :: (MonadIO m, MonadMask m, MonadReader Settings m) => m ()
poll = forever $ do 
    bracket_
        (setFlag settingsBusy)
        (unsetFlag settingsBusy)
        (Log.info ("poll" :: String) >> (liftIO . C.threadDelay) 100000)

    -- wait some time for signal handler to run
    liftIO . C.threadDelay $ 10000

