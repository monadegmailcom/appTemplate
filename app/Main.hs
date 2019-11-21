module Main where

import qualified App
import qualified App.Impl
import           Control.Monad.Reader (runReaderT)

-- application entry point, start with resource for effect implementations
main :: IO ()
main = App.Impl.defaultResource >>= runReaderT App.app

