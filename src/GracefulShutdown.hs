{- | Implement graceful shutdown. Some IO actions should not be interrupted by shutdown. -}
module GracefulShutdown
    ( HasGracefulShutdown(..)
    , createSyncPrimitives
    , guard
    , isScheduled
    , request
    , waitFor
    ) where

import qualified Control.Concurrent.STM as STM
import           Control.Exception.Safe (MonadMask, bracket)
import           Control.Monad (liftM2)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader (MonadReader, asks)
import qualified Data.Unique as Unique

-- | Typeclass for shutdown functionalities.
class HasGracefulShutdown a where
    getShutdown :: a -> STM.TVar Bool -- ^ Indicate shutdown request.
    getRegistry :: a -> STM.TVar [Unique.Unique] -- ^ Pending guarded actions.

-- | Constraint for shutdown functions.
type Constraint env m = (MonadIO m, MonadReader env m, HasGracefulShutdown env)

-- | Suspend shutdown until action finishes.
guard :: (MonadMask m, Constraint env m) => m a -> m a
guard action = bracket
    register
    (maybe (return ()) unregister)
    (const action)

-- | Request shutdown.
request :: Constraint env m => m ()
request = asks getShutdown >>= liftIO . STM.atomically . flip STM.writeTVar True

-- | Wait for shutdown completion, shutdown requested and all pending transactions finished.
waitFor :: Constraint env m => m ()
waitFor = do
    shutdownFlag <- asks getShutdown
    registry <- asks getRegistry
    -- note: both transactional variables are accessed in one STM (software transactional memory),
    -- so they are unaltered from outside if the transaction succeeds
    liftIO . STM.atomically $
            (STM.check =<< STM.readTVar shutdownFlag)
         >> (STM.check =<< null <$> STM.readTVar registry)

-- | Shutdown requested?
isScheduled :: Constraint env m => m Bool
isScheduled = asks getShutdown >>= liftIO . STM.readTVarIO

-- | Create synchronization primitives.
createSyncPrimitives :: IO (STM.TVar [Unique.Unique], STM.TVar Bool)
createSyncPrimitives = STM.atomically $ liftM2 (,) (STM.newTVar []) (STM.newTVar False)

-- register transaction, reject if shutdown requested
register :: Constraint env m => m (Maybe Unique.Unique)
register = do
    shutdownFlag <- asks getShutdown
    registry <- asks getRegistry
    uniqueId <- liftIO Unique.newUnique
    -- note: both transactional variables are accessed in one STM (software transactional memory),
    -- so they are unaltered from outside if the transaction succeeds
    liftIO . STM.atomically $
        STM.readTVar shutdownFlag >>= \case
            True -> return Nothing
            False -> STM.modifyTVar' registry (uniqueId :) >> (return . Just) uniqueId

-- unregister transaction
unregister :: Constraint env m => Unique.Unique -> m ()
unregister uniqueId =
    asks getRegistry >>= liftIO . STM.atomically . flip STM.modifyTVar' (remove uniqueId)
  where
    remove = filter . (/=)

