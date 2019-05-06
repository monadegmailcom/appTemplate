{- Application's state. -}
module State
    ( State(..)
    , defaultState
    , incCount
    ) where

import qualified Control.Concurrent.STM as STM

-- | The application's state.
newtype State = State
    { stateCount :: STM.TVar Int }

-- | Default state with 0 initialized counter.
defaultState :: IO State
defaultState = State <$> STM.newTVarIO 0

-- | Increment and return count
incCount :: State -> STM.STM Int
incCount state = STM.modifyTVar count (+ 1) >> STM.readTVar count
  where
    count = stateCount state
