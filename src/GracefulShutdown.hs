{- | Implement graceful shutdown. Some IO actions should not be interrupted by shutdown. -}
module GracefulShutdown
    ( HasGracefulShutdown(..)
    , createSyncPrimitives
    , request
    , isScheduled
    , suspendWhile
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
    getTransactions :: a -> STM.TVar [Unique.Unique] -- ^ Pending transactions.

-- | Constraint for shutdown functions.
type Constraint env m = (MonadIO m, MonadReader env m, HasGracefulShutdown env)

-- | Suspend shutdown until action finishes.
suspendWhile :: (MonadMask m, Constraint env m) => m a -> m a
suspendWhile action = bracket
    register
    (maybe (return ()) unregister)
    (const action)

-- | Wait for shutdown completion, shutdown requested and all pending transactions finished.
waitFor :: Constraint env m => m ()
waitFor = do
    shutdownFlag <- asks getShutdown
    transactions <- asks getTransactions
    liftIO . STM.atomically $
            (STM.check =<< STM.readTVar shutdownFlag)
         >> (STM.check =<< null <$> STM.readTVar transactions)

-- | Register transaction, reject if shutdown requested.
register :: Constraint env m => m (Maybe Unique.Unique)
register = do
    shutdownFlag <- asks getShutdown
    transactions <- asks getTransactions
    uniqueId <- liftIO Unique.newUnique
    liftIO . STM.atomically $ STM.readTVar shutdownFlag >>= \case
        True -> return Nothing
        False -> STM.modifyTVar' transactions (uniqueId :) >> (return . Just) uniqueId

-- | Unregister transaction.
unregister :: Constraint env m => Unique.Unique -> m ()
unregister uniqueId =
    asks getTransactions >>= liftIO . STM.atomically . flip STM.modifyTVar' (remove uniqueId)
  where
    remove = filter . (/=)

-- | Request shutdown.
request :: Constraint env m => m ()
request = asks getShutdown >>= liftIO . STM.atomically . flip STM.writeTVar True

-- | Shutdown requested?
isScheduled :: Constraint env m => m Bool
isScheduled = asks getShutdown >>= liftIO . STM.readTVarIO

createSyncPrimitives :: IO (STM.TVar [Unique.Unique], STM.TVar Bool)
createSyncPrimitives = STM.atomically $ liftM2 (,) (STM.newTVar []) (STM.newTVar False)
