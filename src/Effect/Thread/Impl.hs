{- | Threading effect implementation. -}
module Effect.Thread.Impl
    () where

import qualified Control.Concurrent as C
import qualified Control.Concurrent.Async as CA
import qualified Control.Exception.Safe as E
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Control (MonadBaseControl, StM, control, liftBaseWith, restoreM)
import           Effect.Thread
import qualified Time.Units

instance (Monad m, MonadIO m, MonadBaseControl IO m, StM m () ~ ()) => ThreadM m where
    myThreadId = liftIO C.myThreadId
    throwTo = E.throwTo
    delay = Time.Units.threadDelay
    timeout time action = liftBaseWith (\run -> Time.Units.timeout time (run action)) >>= \case
        Nothing -> return Nothing
        Just stm -> Just <$> restoreM stm
    mapConcurrently f ls = control $ \runInIO -> CA.mapConcurrently_ (runInIO . f) ls

