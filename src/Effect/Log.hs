{- | Logging effect. -}
module Effect.Log
    ( Level(..)
    , LogM(..)
    , levels
    , debug
    , error
    , info
    , warning
    ) where

import qualified Data.Text.Lazy as TL

import Prelude hiding (log, error)

-- | Ordered log levels.
data Level = Debug | Info | Warning | Error deriving (Eq, Ord, Show)

-- | Available log levels with serialization.
levels :: [(Level, TL.Text)]
levels =
    [ (Debug, "Debug")
    , (Info, "Info")
    , (Warning, "Warning")
    , (Error, "Error")
    ]

-- | Logging effect.
class Monad m => LogM m where
    log :: Level -> TL.Text -> m () -- ^ log msg with level

-- | Log debug level.
debug :: LogM m => TL.Text -> m ()
debug = log Debug

-- | Log info level.
info :: LogM m => TL.Text -> m ()
info = log Info

-- | Log warning level.
warning :: LogM m => TL.Text -> m ()
warning = log Warning

-- | Log error level.
error :: LogM m => TL.Text -> m ()
error = log Error
