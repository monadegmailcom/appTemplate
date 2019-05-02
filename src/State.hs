{- Application's state. -}
module State
    ( HasState(..)
    , State(..)
    , defaultState
    , incCount
    ) where

import qualified Control.Concurrent.STM as STM
import           Control.Monad.IO.Class (liftIO, MonadIO)
import qualified MultiReader as MR

-- | Typeclass for 'State' access.
class HasState m where
    getState :: m State

-- make it an instance of multireader
instance MR.Constraint m State => HasState m where
    getState = MR.ask

-- | The application's state.
newtype State = State
    { stateCount :: STM.TVar Int }

-- | Default state with 0 initialized counter.
defaultState :: MonadIO m => m State
defaultState = liftIO $ State <$> STM.newTVarIO 0

-- | Increment and return count
incCount :: (MonadIO m, HasState m) => m Int
incCount = do
    count <- stateCount <$> getState
    liftIO . STM.atomically $ STM.modifyTVar count (+ 1) >> STM.readTVar count
