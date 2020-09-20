{- | Command line effect. -}
module Effect.CmdLine
    ( CmdLineM(..)
    , CommandLineOptions(..)
    , parseCommandLineOptions
    ) where

import qualified Data.Version as Version
import qualified Paths_appTemplate as Paths
import qualified System.Console.CmdArgs as CA
import           System.Console.CmdArgs ((&=))

-- | Command line options.
data CommandLineOptions = CommandLineOptions
    { cmdLineConfigFile :: FilePath } deriving (Show, CA.Data, CA.Typeable, Eq)
{- HLINT ignore CommandLineOptions -}

-- | Command line effect.
class Monad m => CmdLineM m where
    cmdArgs :: CA.Data a => a -> m a

parseCommandLineOptions :: CmdLineM m => m CommandLineOptions
parseCommandLineOptions = cmdArgs options
  where
    options = CommandLineOptions
        { cmdLineConfigFile
            = defaultConfigFile &= CA.typFile
           &= CA.explicit &= CA.name "c" &= CA.name "config"
           &= CA.help "Path to configuration file" }
        &= CA.summary (procName <> " " <> version)
    procName = "appTemplate"
    version = Version.showVersion Paths.version
    defaultConfigFile = "appTemplate.ini"
