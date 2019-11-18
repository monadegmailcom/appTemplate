{-# OPTIONS_GHC -fno-warn-orphans #-}

{- | Database effect implementation. -}
module Effect.Database.Impl
    ( Exception(..)
    , Redis(..)
    , RedisM(..)
    , runRedis
    ) where

import qualified Config
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception.Safe as E
import           Control.Monad ((>=>))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Char8 as BS
import qualified Database.Redis as Redis
import           Effect.Database
import           Effect.Database.Init

-- | Implementations of database operations.
data Redis = Redis { redisConnection :: !(STM.TVar Redis.Connection)
                   , redisConfig :: !Config.Redis
                   }

class RedisM m where
    get :: m Redis
    set :: Redis -> m ()

instance (Monad m, E.MonadThrow m, E.MonadCatch m, MonadIO m, RedisM m) => DatabaseM m where
    getByKey = runRedis . Redis.get
    setByKey key value = runRedis (Redis.set key value) >>= \case
        Redis.Ok -> return ()
        status -> E.throw . Exception . Redis.Error . BS.pack . show $ status

instance (Monad m, E.MonadThrow m, MonadIO m, RedisM m) => InitM m where
    init config = do
        connection <- liftIO $ Redis.connect (Config.redisConnectInfo config) >>= STM.newTVarIO
        set $ Redis connection config

-- | Wrap Redis.Reply in exception.
newtype Exception = Exception Redis.Reply deriving (Eq, Show, E.Exception)

-- | Run redis action and retry once on failure.
runRedis :: (E.MonadThrow m, E.MonadCatch m, RedisM m, MonadIO m)
         => Redis.Redis (Either Redis.Reply a) -> m a
runRedis action = get >>= retryOnce reconnect . liftIO . run
                      >>= throwOnLeft
  where
    run = STM.readTVarIO . redisConnection >=> flip Redis.runRedis action
    reconnect _ = do
        Redis connection (Config.Redis connectInfo) <- get
        liftIO $ Redis.connect connectInfo >>= STM.atomically . STM.writeTVar connection

-- execute action, handle exception (if any) and retry once
retryOnce :: E.MonadCatch m => (E.SomeException -> m ()) -> m a -> m a
retryOnce handleException action =
    E.tryAny action >>= either (handleException >=> const action) return

-- redis reports errors in a Reply type, just throw it and handle it on the client side
throwOnLeft :: E.MonadThrow m => Either Redis.Reply a -> m a
throwOnLeft = either (E.throw . Exception) return

