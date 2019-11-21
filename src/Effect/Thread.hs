{- | Threading effect. -}
module Effect.Thread
    ( ThreadM(..)) where

import qualified Control.Concurrent as C
import qualified Control.Exception.Safe as E
import qualified Time.Units

type Second = Time.Units.Time Time.Units.Second

-- | Thread effect.
class Monad m => ThreadM m where
    myThreadId :: m C.ThreadId
    throwTo :: E.Exception e => C.ThreadId -> e -> m ()
    delay :: Second -> m ()
    timeout :: Second -> m a -> m (Maybe a)
    mapConcurrently :: (a -> m b) -> [a] -> m ()
