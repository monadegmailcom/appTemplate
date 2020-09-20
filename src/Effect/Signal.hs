{- | Signal effect. -}
module Effect.Signal
    ( SignalM(..)
    , PS.SignalInfo(..)
    ) where

import qualified System.Posix.Signals as PS

-- | Signal effect.
class Monad m => SignalM m where
    installHandler :: (PS.SignalInfo -> m ()) -> PS.Signal -> m ()
