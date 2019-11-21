{- | Filesystem effect. -}
module Effect.Filesystem
    ( FilesystemM(..)) where

import qualified Data.Text as T

-- | Filesystem effect.
class Monad m => FilesystemM m where
    readFile :: FilePath -> m T.Text
