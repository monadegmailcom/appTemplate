module Effect.Concurrent.Stream.Impl
    () where

import           Effect.Concurrent.Stream

import qualified Control.Exception.Safe as E
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Control (MonadBaseControl)
import qualified Streamly as S
import qualified Streamly.Prelude as S

instance (Monad m, E.MonadThrow m, MonadIO m, MonadBaseControl IO m) => StreamM m where
    parallel = S.parallel
    unfoldrM = S.unfoldrM
    repeatM = S.repeatM
    mapM = S.mapM

