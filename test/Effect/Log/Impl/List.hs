module Effect.Log.Impl.List
    ( HasResource(..)
    , Resource(..)
    ) where

import           Effect.Log
import           Effect.Log.Init

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.IORef
import qualified Data.Text.Lazy as TL

data Resource = Resource { resourceSink :: [(Level, TL.Text)]
                         , resourceMinLogLevel :: Level
                         }

class HasResource m where
    getResource :: m (IORef Resource)

instance (Monad m, MonadIO m, HasResource m) => LogM m where
    log level msg = do
        ioRef <- getResource
        liftIO . atomicModifyIORef' ioRef $ \(Resource sink minLogLevel) -> do
            let sink' = if level >= minLogLevel
                        then (level, msg) : sink
                        else sink
            (Resource sink' minLogLevel, ())

instance (Monad m, MonadIO m, HasResource m) => InitM m where
    init minLogLevel _ =
        getResource >>= liftIO . flip atomicWriteIORef (Resource [] minLogLevel)
