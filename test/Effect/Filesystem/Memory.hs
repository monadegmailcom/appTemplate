module Effect.Filesystem.Memory
    ( HasResource(..)
    , Handle(..)
    ) where

import qualified Effect.Filesystem as FS
import qualified Control.Concurrent as C
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BL
import qualified System.IO as FH

class HasResource m where
    getResource :: m (C.MVar (Map.Map FilePath [BL.ByteString]))

data Handle = Handle FilePath FH.IOMode

instance (Monad m, MonadIO m, HasResource m) => FS.FilesystemM m Handle where
    openFile fp fh = return $ Handle fp fh
    hFlush = const $ return ()
    hClose = const $ return ()
    hGetContents (Handle fp _)
        = getResource
      >>= liftIO . C.readMVar
      >>= maybe (error "not found")
                (return . BL.intercalate "\n" . reverse)
        . Map.lookup fp
    hPutStr (Handle fp fh) str = do
        let op = case fh of
                FH.WriteMode -> Map.insert fp [str]
                FH.AppendMode -> Map.alter (Just . maybe [str] (str :)) fp
                _ -> error "cannot write"
        getResource >>= liftIO . (`C.modifyMVar_` (return . op))
    stdout = return $ Handle "stdout" FH.AppendMode
    stderr = return $ Handle "stderr" FH.AppendMode


