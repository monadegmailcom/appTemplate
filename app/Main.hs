module Main where

import qualified App
import           Control.Monad.Reader (runReaderT)
import qualified Environment

-- entry point of process, start application with production environment
main :: IO ()
main = Environment.create >>= runReaderT (App.installSignalHandlers >> App.run)
