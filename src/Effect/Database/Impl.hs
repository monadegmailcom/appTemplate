{-# OPTIONS_GHC -fno-warn-orphans #-}

{- | Database effect implementation. -}
module Effect.Database.Impl
    ( Exception(..)
    , HasDatabase(..)
    , Redis(..)
    , connect
    ) where

import qualified Config
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception.Safe as E
import           Control.Monad ((>=>))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Char8 as BS
import qualified Database.Redis as Redis
import           Effect.Database
-- import qualified Time.Unit

-- | Implementations of database operations.
data Redis = Redis { redisConnection :: !Redis.Connection
                   , redisConfig :: !Config.Redis
                   }

-- | Provide Redis implementation.
class HasDatabase m where
    getRedis :: m (STM.TVar Redis) -- ^ Get redis implementation.

instance (Monad m, E.MonadThrow m, E.MonadCatch m, MonadIO m, HasDatabase m) => DatabaseM m where
    getByKey = runRedis . Redis.get
    setByKey key value = runRedis (Redis.set key value) >>= \case
        Redis.Ok -> return ()
        status -> E.throw . Exception . Redis.Error . BS.pack . show $ status

-- | Wrap Redis.Reply in exception.
newtype Exception = Exception Redis.Reply deriving (Eq, Show, E.Exception)

connect :: MonadIO m => Redis.ConnectInfo -> m Redis.Connection
connect = liftIO . Redis.checkedConnect

runRedis :: (E.MonadThrow m, E.MonadCatch m, HasDatabase m, MonadIO m)
         => Redis.Redis (Either Redis.Reply a) -> m a
runRedis action = getRedis >>= retryOnce reconnect . liftIO . run
                           >>= throwOnLeft
  where
    run = STM.readTVarIO >=> flip Redis.runRedis action . redisConnection
    reconnect _ = do
        tvar <- getRedis
        connection <- (liftIO . STM.readTVarIO) tvar
                  >>= connect . Config.redisConnectInfo . redisConfig
        liftIO . STM.atomically . STM.modifyTVar tvar
            $ (\redis -> redis { redisConnection = connection })

-- execute action, handle exception (if any) and retry once
retryOnce :: E.MonadCatch m => (E.SomeException -> m ()) -> m a -> m a
retryOnce handleException action =
    E.tryAny action >>= either (handleException >=> const action) return

-- redis reports errors in a Reply type, just throw it and handle it on the client side
throwOnLeft :: E.MonadThrow m => Either Redis.Reply a -> m a
throwOnLeft = either (E.throw . Exception) return

