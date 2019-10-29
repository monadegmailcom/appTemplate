{- | Threading effect implementation. -}
module Effect.Thread.Impl
    ( delay ) where

import qualified Control.Concurrent as C
import           Control.Monad.IO.Class (MonadIO, liftIO)

-- | Delay milliseconds.
delay :: MonadIO m => Int -> m ()
delay = liftIO . C.threadDelay

