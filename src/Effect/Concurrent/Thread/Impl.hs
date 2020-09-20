{- | Threading effect implementation. -}
module Effect.Concurrent.Thread.Impl
    () where

import           Effect.Concurrent.Thread

import qualified Control.Concurrent as C
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Control (MonadBaseControl)
import qualified Time.Units

instance (Monad m, MonadIO m, MonadBaseControl IO m) => ThreadM m where
    myThreadId = liftIO C.myThreadId
    delay = Time.Units.threadDelay
