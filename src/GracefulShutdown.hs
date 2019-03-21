{- | Implement graceful shutdown. Address following requirements:

     - 'request' a shutdown at any time. This 'request' may be triggered by receiving an interceptable
       termination signals (e.g. send by systemctl) or other events.
     - Atomicity: you can 'guard' a IO action, so it cannot be interrupted by a request to shutdown,
       the shutdown is delayed until all 'guard'ed IO actions have finished.
     - Do not start new IO actions when a shutdown 'isScheduled'.
     - For a graceful shutdown you 'wait' until all guarded IO actions have finished.
-}
module GracefulShutdown
    ( Constraint
    , HasGracefulShutdown(..)
    , Guard
    , createGuard
    , guard
    , isScheduled
    , request
    , wait
    ) where

import qualified Control.Concurrent.STM as STM
import           Control.Exception.Safe (MonadMask, bracket)
import           Control.Monad (when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader (MonadReader, asks)

-- | Typeclass for shutdown functionalities.
class HasGracefulShutdown a where
    getGuard :: a -> STM.TVar Guard -- ^ Shutdown guard.

{- | Guard the execution of an IO action

     - do not interrupt action by shutdown
     - do not execute new actions when shutdown is requested
     - do not shutdown until all pending actions have finished
-}
data Guard = Guard
    { getShutdownFlag :: !Bool -- ^ Shutdown requested.
    , getPendingCount :: !Int -- ^ Count pending actions.
    } deriving Eq

-- | Constraint for shutdown functions.
type Constraint env m = (MonadIO m, MonadReader env m, HasGracefulShutdown env)

-- lift atomically
atomically :: MonadIO m => STM.STM a -> m a
atomically = liftIO . STM.atomically

-- | Suspend shutdown until action finishes. Return Nothing if action is not
--   executed because of requested shutdown.
guard :: (MonadMask m, Constraint env m) => m a -> m (Maybe a)
guard action = do
    guardTVar <- asks getGuard
    bracket
        (atomically . register $ guardTVar)
        (\allowed -> when allowed (atomically . unregister $ guardTVar))
        (\allowed -> if allowed then Just <$> action else return Nothing)
  where
    -- register action, reject if shutdown requested
    register guardTVar = do
       allowed <- not . getShutdownFlag <$> STM.readTVar guardTVar
       when allowed $ STM.modifyTVar guardTVar (incrPendingCount 1)
       return allowed
    unregister = flip STM.modifyTVar' $ incrPendingCount (-1)
    incrPendingCount inc g = g { getPendingCount = inc + getPendingCount g }

-- | Request shutdown.
request :: Constraint env m => m ()
request = do
    guardTVar <- asks getGuard
    atomically . STM.modifyTVar guardTVar $ (\g -> g { getShutdownFlag = True })

-- | Wait for shutdown completion, return if shutdown requested and all pending transactions
--   finished.
wait :: Constraint env m => m ()
wait = do
    guardTVar <- asks getGuard
    atomically $ STM.readTVar guardTVar >>= STM.check . (== Guard True 0)

-- | Shutdown requested?
isScheduled :: Constraint env m => m Bool
isScheduled = do
    guardTVar <- asks getGuard
    atomically $ getShutdownFlag <$> STM.readTVar guardTVar

-- | Create synchronization primitive.
createGuard :: IO (STM.TVar Guard)
createGuard = STM.atomically $ STM.newTVar (Guard False 0)

