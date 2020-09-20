module Effect.Concurrent.Stream
    ( StreamM(..)
    , timeout
    ) where

import qualified Effect.Concurrent.Thread as Thread

import           Control.Monad (join)
import qualified Streamly as S
import qualified Streamly.Prelude as S

class Monad m => StreamM m where
    parallel :: S.IsStream t => t m a -> t m a -> t m a
    unfoldrM :: S.IsStream t => (b -> m (Maybe (a, b))) -> b -> t m a
    repeatM :: S.IsStream t => m a -> t m a
    mapM :: S.IsStream t => (a -> m b) -> t m a -> t m b

timeout :: (Thread.ThreadM m, StreamM m) => Thread.Second -> m a -> m (Maybe a)
timeout s m = join <$> S.head stream'
  where
    stream' = S.yieldM (Thread.delay s >> return Nothing) `parallel` S.yieldM (Just <$> m)

