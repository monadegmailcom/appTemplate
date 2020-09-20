{- | Threading effect. -}
module Effect.Concurrent.Thread
    ( ThreadM(..)
    , C.ThreadId
    , Second
    ) where

import qualified Control.Concurrent as C
import           Prelude hiding (take)
import qualified Time.Units

type Second = Time.Units.Time Time.Units.Second

-- | Thread effect.
class Monad m => ThreadM m where
    myThreadId :: m C.ThreadId
    delay :: Second -> m ()

