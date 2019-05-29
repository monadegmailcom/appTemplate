{- | Application's state. -}
module Effect.State
    ( StateM(..)
    ) where

-- | State effect.
class Monad m => StateM m where
    getCount :: m Int -- ^ read counter
    incCount :: m () -- ^ increment count

