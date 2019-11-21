{- | Application state effect. -}
module Effect.State
    ( StateM(..)
    ) where

-- | State effect.
class Monad m => StateM m where
    getCount :: m Int -- ^ Read counter.
    incCount :: m () -- ^ Increment count.

