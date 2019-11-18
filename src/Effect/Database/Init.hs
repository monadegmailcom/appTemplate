{- | Database initialize effect -}
module Effect.Database.Init
    ( InitM(..)) where

import qualified Config

-- | Database initialize effect.
class Monad m => InitM m where
    init :: Config.Redis -> m ()

