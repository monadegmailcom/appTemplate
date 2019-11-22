{- | Implementation of filesystem effects. -}
module Effect.Filesystem.Impl () where

import           Effect.Filesystem

import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text.Lazy.IO as TL

instance (Monad m, MonadIO m) => FilesystemM m where
    readFile = liftIO . TL.readFile
