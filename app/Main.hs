module Main where

import qualified App.Impl
import qualified Config
import qualified Control.Concurrent as C
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as E
import qualified Control.Exception.Safe as E.Safe
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (runReaderT)
import           Control.Monad.Trans.Control (control)
import qualified Data.Text.IO as T
import qualified Data.Version as Version
import qualified Database.Redis as Redis
import qualified Effect.Database.Impl as Database
import qualified Effect.CmdLine as CmdLine
import qualified Effect.CmdLine.Impl as CmdLine ()
import qualified Effect.Log as Log
import qualified Effect.Log.Impl as Log
import qualified Effect.State.Impl as State
import           Formatting ((%))
import qualified Formatting as F
import qualified Paths_appTemplate as Paths

-- Customize ini file related exceptions
data IniFileException = IniFileException String deriving (Show, E.Exception)

-- entry point, start application from IO
main :: IO ()
main = App.Impl.initializeEnv >>= runReaderT (App.Impl.app "")

