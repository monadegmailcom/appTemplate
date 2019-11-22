{- | Filesystem effect. -}
module Effect.Filesystem
    ( FilesystemM(..)) where

import qualified Data.Text.Lazy as TL

-- | Filesystem effect.
class Monad m => FilesystemM m where
    readFile :: FilePath -> m TL.Text
