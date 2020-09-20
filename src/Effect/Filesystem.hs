{- | Filesystem effect. -}
module Effect.Filesystem
    ( FilesystemM(..)
    , FH.IOMode(..)
    ) where

import qualified Data.ByteString.Lazy as BL

import qualified System.IO as FH

-- | Filesystem effect.
class Monad m => FilesystemM m h | m -> h where
    openFile :: FilePath -> FH.IOMode -> m h
    hFlush :: h -> m ()
    hClose :: h -> m ()
    hGetContents :: h -> m BL.ByteString
    hPutStr :: h -> BL.ByteString -> m ()
    stdout :: m h
    stderr :: m h
