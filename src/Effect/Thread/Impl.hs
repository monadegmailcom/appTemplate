{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE TypeFamilies #-}

{- | Threading effect implementation. -}
module Effect.Thread.Impl
    () where

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Control (MonadBaseControl, liftBaseWith, restoreM)
import           Effect.Thread
import qualified Time.Units

instance (Monad m, MonadIO m,  MonadBaseControl IO m) => ThreadM m where
    delay = Time.Units.threadDelay
    timeout time action = liftBaseWith (\run -> Time.Units.timeout time (run action)) >>= \case
        Nothing -> return Nothing
        Just stm -> Just <$> restoreM stm

