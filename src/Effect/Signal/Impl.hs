{- | Signal effect implementation -}
module Effect.Signal.Impl () where

import           Effect.Signal

import           Control.Monad (void)
import           Control.Monad.Trans.Control (MonadBaseControl, StM, control)

import qualified System.Posix.Signals as PS

instance (Monad m, MonadBaseControl IO m, StM m () ~ ()) => SignalM m where
    installHandler handler signal = control $ \runInIO -> void $ PS.installHandler
        signal
        (PS.CatchInfo $ \signalInfo -> runInIO (handler signalInfo))
        Nothing
