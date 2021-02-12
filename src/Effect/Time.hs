{- | Time effect. -}
module Effect.Time
    (TimeM(..)) where

import qualified Data.Time.LocalTime as LocalTime

-- | Time effect.
class Monad m => TimeM m where
    getZonedTime :: m LocalTime.ZonedTime
