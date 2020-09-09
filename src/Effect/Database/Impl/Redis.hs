{- | Database effect implementation. -}
module Effect.Database.Impl.Redis
    ( Exception(..)
    , HasResource(..)
    , Redis.Connection
    , Resource
    , def
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
    getResource :: m Resource

newtype Resource = Resource { resourceConnection :: C.MVar (Maybe Redis.Connection)}

def :: IO Resource
def = Resource <$> C.newMVar Nothing

instance (Monad m, E.MonadThrow m, E.MonadCatch m, MonadIO m, HasResource m)
  => DatabaseM Config.Redis m where
    connect config = do
        connection <- liftIO $ Redis.checkedConnect (Config.redisConnectInfo config)
        getResource >>= \(Resource mvar) -> liftIO . void $ C.swapMVar mvar (Just connection)
    getByKey = runRedis . Redis.get
    setByKey key value = runRedis (Redis.set key value) >>= \case
        Redis.Ok -> return ()
        status -> E.throw . Exception . Redis.Error . BS.pack . show $ status

-- | Wrap Redis.Reply in exception.
newtype Exception = Exception Redis.Reply deriving (Eq, Show, E.Exception)

-- run redis action and retry once on failure.
runRedis :: (E.MonadThrow m, E.MonadCatch m, MonadIO m, HasResource m)
         => Redis.Redis (Either Redis.Reply a) -> m a
runRedis action = getResource >>= liftIO . C.readMVar . resourceConnection >>= \case
    Nothing -> E.throw $ Exception $ Redis.Error "Redis uninitialized"
    Just con -> liftIO (Redis.runRedis con action) >>= throwOnLeft

-- redis reports errors in a Reply type, just throw it and handle it on the client side
throwOnLeft :: E.MonadThrow m => Either Redis.Reply a -> m a
throwOnLeft = either (E.throw . Exception) return

