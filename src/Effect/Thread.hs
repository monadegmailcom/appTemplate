{- | Threading effect. -}
module Effect.Thread
    ( ThreadM(..)) where

-- | Thread effect.
class Monad m => ThreadM m where
    delay :: Int -> m () 
