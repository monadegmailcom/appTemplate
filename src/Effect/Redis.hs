{- | Database effect. -}
module Effect.Redis
    ( RedisM(..)
    , get
    , set) where

import qualified Control.Exception.Safe as E
import qualified Data.ByteString as BS
import qualified Database.Redis as Redis

-- | Database effect.
class Monad m => RedisM m c | m -> c where
    connect :: Redis.ConnectInfo -> m c -- ^ Connect to database
    runRedis :: c -> Redis.Redis a -> m a

-- | Wrap Redis.Reply or Status in exception.
data Exception = Reply Redis.Reply
               | Status BS.ByteString deriving (Eq, Show)
                                      deriving anyclass (E.Exception)

-- redis reports errors in a Reply type, just throw it and handle it on the client side
throwOnLeft :: E.MonadThrow m => Either Redis.Reply a -> m a
throwOnLeft = either (E.throw . Reply) return

throwOnStatus :: E.MonadThrow m => Redis.Status -> m ()
throwOnStatus = \case
    Redis.Status bs -> E.throw $ Status bs
    _ -> return ()

get :: (E.MonadThrow m, RedisM m c)
    => c
    -> BS.ByteString
    -> m (Maybe BS.ByteString)
get con key
    = runRedis con (Redis.get key)
  >>= throwOnLeft

set :: (E.MonadThrow m, RedisM m c)
    => c
    -> BS.ByteString
    -> BS.ByteString
    -> m ()
set con key value
    = runRedis con (Redis.set key value)
  >>= throwOnLeft
  >>= throwOnStatus
