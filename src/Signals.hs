{- | Handle signals sent to application. -}
module Signals
    ( installHandler
    , terminateSignals
    ) where

import           Control.Monad (void)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified System.Posix.Signals as PS

-- | Install signal handler.
installHandler :: MonadIO m => IO () -> PS.Signal -> m ()
installHandler handler signal = liftIO . void $ PS.installHandler
    signal
    (PS.Catch handler)
    Nothing

-- | List of signals on which we want to terminate, this conforms to the unix defaults,
-- see http://man7.org/linux/man-pages/man7/signal.7.html
terminateSignals :: [PS.Signal]
terminateSignals = [PS.sigHUP, PS.sigINT, PS.sigPIPE, PS.sigALRM, PS.sigTERM]

