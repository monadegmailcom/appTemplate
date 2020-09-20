{- | Implementation of filesystem effects. -}
module Effect.Filesystem.Impl (Handle) where

import           Effect.Filesystem

import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Lazy as BL
import qualified System.IO

type Handle = System.IO.Handle

instance (Monad m, MonadIO m) => FilesystemM m Handle where
    openFile fp = liftIO . System.IO.openFile fp
    hFlush = liftIO . System.IO.hFlush
    hClose = liftIO . System.IO.hClose
    hGetContents = liftIO . BL.hGetContents
    hPutStr h = liftIO . BL.hPutStr h
    stdout = return System.IO.stdout
    stderr = return System.IO.stderr
