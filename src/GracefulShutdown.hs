{- | Implement graceful shutdown. Some IO actions should not be interrupted by shutdown. -}
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

     Implementation details

     - contains a Either Int Int
     - both Ints count the pending actions
     - a Left indicates a requested shutdown
     - a Right indicates no shutdown requested
-}
newtype Guard = Guard { getEither :: Either Int Int } deriving Eq

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
        (\registered -> when registered (atomically . unregister $ guardTVar))
        (\registered -> if registered then Just <$> action
                                      else return Nothing)
  where
    -- register action, reject if shutdown requested
    register guardTVar = getEither <$> STM.readTVar guardTVar >>= \case
       Left _ -> return False
       Right count -> do
          STM.writeTVar guardTVar (Guard (Right (count + 1)))
          return True
    unregister guardTVar = STM.modifyTVar' guardTVar (Guard . dec . getEither)
    dec = \case
        Left count -> Left (count - 1)
        Right count -> Right (count - 1)

-- | Request shutdown.
request :: Constraint env m => m ()
request = do
    guardTVar <- asks getGuard
    atomically $ STM.modifyTVar guardTVar (Guard . modify . getEither)
  where
    modify = \case
        -- switch from pending count from Left to Right
        Right count -> Left count
        x -> x

-- | Wait for shutdown completion, shutdown requested and all pending transactions finished.
wait :: Constraint env m => m ()
wait = do
    guardTVar <- asks getGuard
    atomically $ STM.readTVar guardTVar >>= STM.check . (== Guard (Left 0))

-- | Shutdown requested?
isScheduled :: Constraint env m => m Bool
isScheduled = do
    guardTVar <- asks getGuard
    atomically $ isLeft . getEither <$> STM.readTVar guardTVar
  where
    isLeft = \case
        Left _ -> True
        Right _ -> False

-- | Create synchronization primitive.
createGuard :: IO (STM.TVar Guard)
createGuard = STM.atomically $ STM.newTVar (Guard (Right 0))

