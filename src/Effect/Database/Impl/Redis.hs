{- | Database effect implementation. -}
module Effect.Database.Impl.Redis
    ( Exception(..)
    , Resource(..)
    , HasResource(..)
    ) where

import qualified Config
import qualified Effect.Log as Log
import           Effect.Database
import           Effect.Database.Init

import qualified Control.Concurrent as C
import qualified Control.Exception.Safe as E
import           Control.Monad ((>=>))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Lazy as TL
import qualified Database.Redis as Redis

-- | Database resource.
data Resource = Resource { resourceConnection :: !Redis.Connection -- ^ Redis connection.
                         , resourceConfig :: !Config.Redis -- ^ Configuration for reconnect.
                         }

-- | Provide resource.
class HasResource m where
    getResource :: m (C.MVar Resource)

instance (Monad m, E.MonadThrow m, E.MonadCatch m, MonadIO m, Log.LogM m, HasResource m)
    => DatabaseM m where
    getByKey = runRedis . Redis.get
    setByKey key value = runRedis (Redis.set key value) >>= \case
        Redis.Ok -> return ()
        status -> E.throw . Exception . Redis.Error . BS.pack . show $ status

instance (Monad m, MonadIO m, HasResource m) => InitM m where
    init config = do
        connection <- liftIO $ Redis.checkedConnect (Config.redisConnectInfo config)
        getResource >>= liftIO . (`C.putMVar` Resource connection config)

-- | Wrap Redis.Reply in exception.
newtype Exception = Exception Redis.Reply deriving (Eq, Show, E.Exception)

-- run redis action and retry once on failure.
runRedis :: (E.MonadThrow m, E.MonadCatch m, MonadIO m, Log.LogM m, HasResource m)
         => Redis.Redis (Either Redis.Reply a) -> m a
runRedis action = getResource >>= liftIO . C.readMVar
                              >>= retryOnce reconnect . run . resourceConnection
                              >>= throwOnLeft
  where
    run = liftIO . flip Redis.runRedis action
    -- note: all concurrent database operations are blocked on purpose while reconnecting
    reconnect e = do
        Log.warning $ "Redis action failed: " <> (TL.pack . show ) e
        Log.info "Reconnect and try again"
        getResource >>= liftIO . flip C.modifyMVar_ updateResource
    updateResource resource = do
        connection <- Redis.connect . Config.redisConnectInfo . resourceConfig $ resource
        return $ resource { resourceConnection = connection }

-- execute action, handle exception (if any) and retry once
retryOnce :: E.MonadCatch m => (E.SomeException -> m ()) -> m a -> m a
retryOnce handleException action = E.catchAny action (handleException >=> const action)

-- redis reports errors in a Reply type, just throw it and handle it on the client side
throwOnLeft :: E.MonadThrow m => Either Redis.Reply a -> m a
throwOnLeft = either (E.throw . Exception) return

