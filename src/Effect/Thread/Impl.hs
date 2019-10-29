{-# OPTIONS_GHC -fno-warn-orphans #-}

{- | Threading effect implementation. -}
module Effect.Thread.Impl
    () where

import qualified Control.Concurrent as C
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Effect.Thread

instance (Monad m, MonadIO m) => ThreadM m where
    delay = liftIO . C.threadDelay

