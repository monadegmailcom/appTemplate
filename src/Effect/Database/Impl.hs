{-# OPTIONS_GHC -fno-warn-orphans #-}

{- | Database effect implementation. -}
module Effect.Database.Impl
    ( Exception(..)
    , HasDatabase(..)
    , Redis(..)) where

import qualified Control.Concurrent.STM as STM
import qualified Control.Exception.Safe as E
import           Control.Monad (when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Char8 as BS
import qualified Database.Redis as Redis
import           Effect.Database

-- | Implementations of database operations.
data Redis = Redis { redisMaster   :: Maybe Redis.Connection
                   , redisSlaves   :: [Redis.Connection]
                   , redisSentinel :: Maybe Redis.Connection }

class HasDatabase m where
    getRedis :: m (STM.TVar Redis) -- ^ Get redis implementation.

instance (Monad m, E.MonadThrow m, MonadIO m, HasDatabase m) => DatabaseM m where
    getByKey key = do
        master <- getMaster
        liftIO (Redis.runRedis master $ Redis.get key) >>= throwReply
    setByKey key value = do
        status <- getMaster >>= runRedis (Redis.set key value)
        when (status /= Redis.Ok) $ E.throw . Exception . Redis.Error . BS.pack . show $ status

newtype Exception = Exception Redis.Reply deriving (Eq, Show, E.Exception)

throwReply :: E.MonadThrow m => Either Redis.Reply a -> m a
throwReply = either (E.throw . Exception) return

runRedis :: (E.MonadThrow m, MonadIO m) 
    => Redis.Redis (Either Redis.Reply a) -> Redis.Connection -> m a
runRedis redis connection = liftIO (Redis.runRedis connection redis) >>= throwReply

getMaster :: (Monad m, E.MonadThrow m, MonadIO m, HasDatabase m) => m Redis.Connection
getMaster = getRedis
        >>= fmap redisMaster . liftIO . STM.readTVarIO
        >>= maybe (E.throwString "no master connection") return

