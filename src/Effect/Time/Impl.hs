{- Time effect implementation. -}
module Effect.Time.Impl
    () where

import           Effect.Time

import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Time.LocalTime as LocalTime

instance (Monad m, MonadIO m) => TimeM m where
    getZonedTime = liftIO LocalTime.getZonedTime

