{- | State effect implementation. -}
module Effect.State.Impl
    ( HasResource(..)
    , Resource
    , defaultResource
    ) where

import qualified Control.Concurrent.STM as STM
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Effect.State

-- | The application state resource.
newtype Resource = Resource
    { resourceCount :: STM.TVar Int }

-- | Provide state implementation.
class HasResource m where
    getResource :: m Resource -- ^ Get current state.

-- | Default state with 0 initialized counter.
defaultResource :: IO Resource
defaultResource = Resource <$> STM.newTVarIO 0

-- implement state read/write in IO using resource
instance (Monad m, MonadIO m, HasResource m) => StateM m where
    getCount = getResource >>= liftIO . STM.atomically . STM.readTVar . resourceCount
    incCount = getResource
           >>= liftIO . STM.atomically . flip STM.modifyTVar' (+ 1) . resourceCount
