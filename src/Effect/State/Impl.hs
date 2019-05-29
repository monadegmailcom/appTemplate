{-# OPTIONS_GHC -fno-warn-orphans #-}

{- | State effect implementation. -}
module Effect.State.Impl
    ( HasState(..)
    , State
    , defaultState
    ) where

import qualified Control.Concurrent.STM as STM
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Effect.State

-- | The application's state.
newtype State = State
    { stateCount :: STM.TVar Int }

-- | Provide state implementation. 
class HasState m where
    getState :: m State -- ^ get current state

-- | Default state with 0 initialized counter.
defaultState :: IO State
defaultState = State <$> STM.newTVarIO 0

-- implement state read/write in IO using 'HasState'
instance (Monad m, MonadIO m, HasState m) => StateM m where
    getCount = getState >>= liftIO . STM.atomically . STM.readTVar . stateCount
    incCount = do
        state <- getState 
        liftIO . STM.atomically $ STM.modifyTVar' (stateCount state) (+ 1)
