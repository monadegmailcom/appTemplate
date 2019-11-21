{- | Logging initialization effect. -}
module Effect.Log.Init 
    ( InitM(..)) where

import qualified Effect.Log as Log

-- | Initialize logging effect.
class Monad m => InitM m where
    init :: Log.Level -> Log.Destination -> m () -- ^ Log to destination, call before logging.

