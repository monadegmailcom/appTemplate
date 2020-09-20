module Effect.Concurrent.STM
    ( STM(..)
    , newEmptyTMVar'
    , newTMVar'
    , readTMVar'
    , tryReadTMVar'
    , takeTMVar'
    , tryTakeTMVar'
    , putTMVar'
    , tryPutTMVar'
    , swapTVar'
    , newTVar'
    , readTVar'
    , writeTVar'
    , newTChan'
    , readTChan'
    , tryReadTChan'
    , writeTChan'
    ) where

import           Data.Kind
import           Prelude hiding (read, take)

class Monad (ST m) => STM m where
    data ST m :: Type -> Type
    data TMVar m :: Type -> Type
    data TVar m :: Type -> Type
    data TChan m :: Type -> Type
    atomically :: ST m a -> m a
    retry :: ST m ()
    newEmptyTMVar :: ST m (TMVar m a)
    newTMVar :: a -> ST m (TMVar m a)
    readTMVar :: TMVar m a -> ST m a
    takeTMVar :: TMVar m a -> ST m a
    tryTakeTMVar :: TMVar m a -> ST m (Maybe a)
    tryReadTMVar :: TMVar m a -> ST m (Maybe a)
    putTMVar :: TMVar m a -> a -> ST m ()
    tryPutTMVar :: TMVar m a -> a -> ST m Bool
    newTChan :: ST m (TChan m a)
    readTChan :: TChan m a -> ST m a
    tryReadTChan :: TChan m a -> ST m (Maybe a)
    writeTChan :: TChan m a -> a -> ST m ()
    newTVar :: a -> ST m (TVar m a)
    readTVar :: TVar m a -> ST m a
    writeTVar :: TVar m a -> a -> ST m ()
    swapTVar :: TVar m a -> a -> ST m a

newEmptyTMVar' :: STM m => m (TMVar m a)
newEmptyTMVar' = atomically newEmptyTMVar

newTMVar' :: STM m => a -> m (TMVar m a)
newTMVar' = atomically . newTMVar

readTMVar' :: STM m => TMVar m a -> m a
readTMVar' = atomically . readTMVar

tryReadTMVar' :: STM m => TMVar m a -> m (Maybe a)
tryReadTMVar' = atomically . tryReadTMVar

takeTMVar' :: STM m => TMVar m a -> m a
takeTMVar' = atomically . takeTMVar

tryTakeTMVar' :: STM m => TMVar m a -> m (Maybe a)
tryTakeTMVar' = atomically . tryTakeTMVar

putTMVar' :: STM m => TMVar m a -> a -> m ()
putTMVar' mvar = atomically . putTMVar mvar

tryPutTMVar' :: STM m => TMVar m a -> a -> m Bool
tryPutTMVar' mvar = atomically . tryPutTMVar mvar

swapTVar' :: STM m => TVar m a -> a -> m a
swapTVar' tvar = atomically . swapTVar tvar

newTVar' :: STM m => a -> m (TVar m a)
newTVar' = atomically . newTVar

{-# ANN readTVar' ("HLint: ignore Use readTVarIO" :: String) #-}
readTVar' :: STM m => TVar m a -> m a
readTVar' = atomically . readTVar

writeTVar' :: STM m => TVar m a -> a -> m ()
writeTVar' tvar = atomically . writeTVar tvar

newTChan' :: STM m => m (TChan m a)
newTChan' = atomically newTChan

readTChan' :: STM m => TChan m a -> m a
readTChan' = atomically . readTChan

tryReadTChan' :: STM m => TChan m a -> m (Maybe a)
tryReadTChan' = atomically . tryReadTChan

writeTChan' :: STM m => TChan m a -> a -> m ()
writeTChan' tchan = atomically . writeTChan tchan
