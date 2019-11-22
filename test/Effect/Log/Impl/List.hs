module Effect.Log.Impl.List
    ( HasResource(..)
    , Resource(..)
    ) where

import           Effect.Log
import           Effect.Log.Init

import qualified Control.Concurrent as C
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text.Lazy as TL

data Resource = Resource { resourceSink :: [(Level, TL.Text)]
                         , resourceMinLogLevel :: Level
                         }

class HasResource m where
    getResource :: m (C.MVar Resource)

instance (Monad m, MonadIO m, HasResource m) => LogM m where
    log level msg = do
        mvar <- getResource
        liftIO . C.modifyMVar_ mvar $ \(Resource sink minLogLevel) -> do
            let sink' = if level >= minLogLevel
                        then (level, msg) : sink
                        else sink
            return $ Resource sink' minLogLevel

instance (Monad m, MonadIO m, HasResource m) => InitM m where
    init minLogLevel _ = getResource >>= liftIO . flip C.putMVar (Resource [] minLogLevel)
