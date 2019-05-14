{-# OPTIONS_GHC -fno-cse #-}

{- | Command line and config file parsing. -}
module Config
    ( CommandLineOptions(..)
    , Config(..)
    , Log(..)
    , parseCommandLineOptions
    , parseConfigFile
    , readConfigFile
    ) where

import           Data.Bifunctor (first)
import qualified Data.CaseInsensitive as CI
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import qualified Data.List as L
import qualified Data.Text.Lazy as TL
import qualified Data.Version as Version
import qualified Log
import qualified Paths_appTemplate as Paths
import           System.Console.CmdArgs ((&=))
import qualified System.Console.CmdArgs as CA

-- | Command line options.
data CommandLineOptions = CommandLineOptions
    { cmdLineConfigFile :: FilePath } deriving (Show, CA.Data, CA.Typeable, Eq)
{- HLINT ignore CommandLineOptions -}

-- | Global configuration.
newtype Config = Config
    { configLog :: Log
    } deriving (Eq, Show)

-- | Logging configuration.
data Log = Log
    { logFile :: !(Maybe FilePath) -- ^ log to stdout if Nothing
    , logLevel :: !Log.Level -- ^ filter messages with minimum level
    } deriving (Eq, Show)

-- | Parse command line options, exit process on "help" or "version" option.
parseCommandLineOptions :: IO CommandLineOptions
parseCommandLineOptions = CA.cmdArgs commandLineOptions
  where
    commandLineOptions = CommandLineOptions
        { cmdLineConfigFile
            = defaultConfigFile &= CA.typFile
           &= CA.explicit &= CA.name "c" &= CA.name "config"
           &= CA.help "Path to configuration file" }
      &= CA.summary ("appTemplate " <> version)
    version = Version.showVersion Paths.version
    defaultConfigFile = "appTemplate.cfg"

-- | Read config file.
readConfigFile :: FilePath -> IO C.Config
readConfigFile filePath = C.load [C.Required filePath]

-- | Parse config file.
parseConfigFile :: C.Config -> IO Config
parseConfigFile = fmap Config . logSectionParser
  where
    logSectionParser cfg =
        let prefix = "Log."
        in Log
            <$> C.lookup cfg (prefix <> "path")
            <*> (C.require cfg (prefix <> "level") >>= toLogLevel)
    -- translate case insensitive string to log level
    toLogLevel str =
        let logLevels = map (first CI.mk . swap) Log.levels
            swap (a,b) = (b,a)
        in case L.lookup (CI.mk str) logLevels of
            Nothing -> fail $
                  "Invalid log level, choose one of "
               <> (TL.unpack . TL.intercalate ", " . map snd) Log.levels
            Just level -> return level

