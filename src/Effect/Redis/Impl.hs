{- | Database effect implementation. -}
module Effect.Redis.Impl
    () where

import           Effect.Redis

import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Database.Redis as Redis

instance (Monad m, MonadIO m) => RedisM m Redis.Connection where
    connect = liftIO . Redis.checkedConnect
    runRedis c = liftIO . Redis.runRedis c
