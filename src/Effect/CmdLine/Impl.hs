{- | Command line effect implementation. -}
module Effect.CmdLine.Impl () where

import           Effect.CmdLine

import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Version as Version
import qualified Paths_appTemplate as Paths
import           System.Console.CmdArgs ((&=))
import qualified System.Console.CmdArgs as CA

instance (Monad m, MonadIO m) => CmdLineM m where
    -- exit process on "help" or "version" option
    parseCommandLineOptions = liftIO $ CA.cmdArgs getCommandLineOptions

getCommandLineOptions :: CommandLineOptions
getCommandLineOptions = CommandLineOptions
    { cmdLineConfigFile
        = defaultConfigFile &= CA.typFile
       &= CA.explicit &= CA.name "c" &= CA.name "config"
       &= CA.help "Path to configuration file" }
    &= CA.summary (procName <> " " <> version)
  where
    procName = "appTemplate"
    version = Version.showVersion Paths.version
    defaultConfigFile = "appTemplate.ini"
