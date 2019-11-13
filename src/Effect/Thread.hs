{- | Threading effect. -}
module Effect.Thread
    ( ThreadM(..)) where

import qualified Time.Units

type Second = Time.Units.Time Time.Units.Second

-- | Thread effect.
class Monad m => ThreadM m where
    delay :: Second -> m ()
    timeout :: Second -> m a -> m (Maybe a)
