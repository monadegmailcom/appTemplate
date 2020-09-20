module Effect.Concurrent.STM.Impl
    () where

import           Effect.Concurrent.STM

import qualified Control.Concurrent.STM as C
import           Control.Monad.IO.Class (MonadIO, liftIO)

instance MonadIO m => STM m where
    newtype ST m a = ST (C.STM a) deriving newtype (Functor, Applicative, Monad)
    newtype TMVar m a = TMVar (C.TMVar a)
    newtype TVar m a = TVar (C.TVar a)
    newtype TChan m a = TChan (C.TChan a)
    atomically (ST stm) = liftIO . C.atomically $ stm
    retry = ST C.retry
    newEmptyTMVar =  ST . fmap TMVar $ C.newEmptyTMVar
    newTMVar = ST . fmap TMVar . C.newTMVar
    readTMVar (TMVar tmvar) = ST . C.readTMVar $ tmvar
    takeTMVar (TMVar tmvar) = ST . C.takeTMVar $ tmvar
    tryTakeTMVar (TMVar tmvar) = ST . C.tryTakeTMVar $ tmvar
    tryReadTMVar (TMVar tmvar) = ST . C.tryReadTMVar $ tmvar
    putTMVar (TMVar tmvar) a = ST $ C.putTMVar tmvar a
    tryPutTMVar (TMVar tmvar) a = ST $ C.tryPutTMVar tmvar a
    newTVar = ST . fmap TVar . C.newTVar
    readTVar (TVar tvar) = ST . C.readTVar $ tvar
    writeTVar (TVar tvar) a = ST $ C.writeTVar tvar a
    swapTVar (TVar tvar) a = ST $ C.swapTVar tvar a
    newTChan =  ST . fmap TChan $ C.newTChan
    readTChan (TChan tchan) = ST . C.readTChan $ tchan
    tryReadTChan (TChan tchan) = ST . C.tryReadTChan $ tchan
    writeTChan (TChan tchan) a = ST $ C.writeTChan tchan a
