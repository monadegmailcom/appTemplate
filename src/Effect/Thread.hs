{- | Threading effect. -}
module Effect.Thread
    ( ThreadM(..)) where

-- | Thread effect.
class Monad m => ThreadM m where
    -- | Delay milliseconds.
    delay :: Int -> m () 
