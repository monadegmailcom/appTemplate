{- | Command line effect implementation. -}
module Effect.CmdLine.Impl () where

import           Effect.CmdLine

import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified System.Console.CmdArgs as CA

instance (Monad m, MonadIO m) => CmdLineM m where
    -- exit process on "help" or "version" option
    cmdArgs = liftIO . CA.cmdArgs

