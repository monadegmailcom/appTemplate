{- | Implementation of filesystem effects. -}
module Effect.Filesystem.Impl () where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Effect.Filesystem
import qualified Data.Text.IO as T

instance (Monad m, MonadIO m) => FilesystemM m where
    readFile = liftIO . T.readFile
