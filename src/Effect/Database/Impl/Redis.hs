{- | Database effect implementation. -}
module Effect.Database.Impl.Redis
    ( Exception(..)
    , HasResource(..)
    , Redis.Connection
    ) where

import qualified Config
import           Effect.Database

import qualified Control.Concurrent as C
import qualified Control.Exception.Safe as E
import           Control.Monad (void)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Char8 as BS
import qualified Database.Redis as Redis

-- | Provide resource.
class HasResource m where
    getResource :: m (C.MVar (Maybe Redis.Connection))

instance (Monad m, E.MonadThrow m, E.MonadCatch m, MonadIO m, HasResource m)
  => DatabaseM Config.Redis m where
    connect config = do
        connection <- liftIO $ Redis.checkedConnect (Config.redisConnectInfo config)
        getResource >>= liftIO . void . flip C.swapMVar (Just connection)
    getByKey = runRedis . Redis.get
    setByKey key value = runRedis (Redis.set key value) >>= \case
        Redis.Ok -> return ()
        status -> E.throw . Exception . Redis.Error . BS.pack . show $ status

-- | Wrap Redis.Reply in exception.
newtype Exception = Exception Redis.Reply deriving (Eq, Show, E.Exception)

-- run redis action and retry once on failure.
runRedis :: (E.MonadThrow m, E.MonadCatch m, MonadIO m, HasResource m)
         => Redis.Redis (Either Redis.Reply a) -> m a
runRedis action = getResource >>= liftIO . C.readMVar >>= \case
    Nothing -> E.throw $ Exception $ Redis.Error "Redis uninitialized"
    Just con -> liftIO (Redis.runRedis con action) >>= throwOnLeft

-- redis reports errors in a Reply type, just throw it and handle it on the client side
throwOnLeft :: E.MonadThrow m => Either Redis.Reply a -> m a
throwOnLeft = either (E.throw . Exception) return

