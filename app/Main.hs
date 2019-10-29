module Main where

import qualified App.Impl
import qualified Config
import qualified Control.Concurrent as C
import qualified Control.Exception as E
import qualified Control.Exception.Safe as E.Safe
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (runReaderT)
import           Control.Monad.Trans.Control (control)
import qualified Data.Text.IO as T
import qualified Data.Version as Version
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
main = do
           -- parse command line for config file
    content <- CmdLine.parseCommandLineOptions
           -- read config file accordingly
       >>= T.readFile . CmdLine.cmdLineConfigFile
           -- parse config file syntactically to ini type
    -- parse configuration semantically from ini type
    (config, prettyContent) <-
        either (E.Safe.throwM . IniFileException) return . Config.parseIniFile $ content
    -- build logger from configuration
    logger <- let Config.Log mPath logLevel = Config.configLog config
              in Log.makeLoggerSet mPath >>= Log.makeLogFunction logLevel
    -- define environment with effect implementations
    env <- App.Impl.Env logger <$> State.defaultState
    flip runReaderT env $ do
        -- log current (read-only) configuration from pretty printed ini type
        Log.info . F.format ("Initial configuration\n" % F.stext) $ prettyContent
        -- termination signals should be thrown as async exception to main thread
        liftIO C.myThreadId >>= App.Impl.installSignalHandlers
        -- log greeting message with version info
        Log.info . F.format ("Startup version " % F.string) $ Version.showVersion Paths.version
        -- call pollers forever and log goodbye message finally
        -- if this thread receives as async exception all pollers are cancelled
        -- if either poller throws an exception all sibling pollers are cancelled
        control $ \runInIO -> E.finally (runInIO App.Impl.runPollers)
                                        (runInIO $ Log.info "Shutdown complete")

