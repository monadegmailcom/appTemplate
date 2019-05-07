{- | Command line and config file parsing. -}
module Config
    ( CommandLineOptions(..)
    , Config(..)
    , Log(..)
    , parseCommandLineOptions
    , parseConfigFile
    ) where

import           Data.Bifunctor (first)
import qualified Data.CaseInsensitive as CI
import qualified Data.Configurator as C
import qualified Data.List as L
import qualified Data.Text.Lazy as TL
import qualified Data.Version as Version
import qualified Log
import qualified Options.Applicative as Options
import qualified Paths_appTemplate as Paths

-- | Command line options.
newtype CommandLineOptions = CommandLineOptions
    { cmdLineConfigFile :: FilePath }

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
parseCommandLineOptions = Options.execParser parserInfo
  where
    parserInfo = Options.info optionModifier Options.fullDesc
    optionModifier = Options.helper
        <*> versionParser
        <*> fmap CommandLineOptions optionsParser
    versionParser = Options.infoOption version
         $ Options.short 'v'
        <> Options.long "version"
        <> Options.help "Show version"
    -- version string, set in package.txt
    version = Version.showVersion Paths.version
    optionsParser = Options.strOption
         $ Options.long "config"
        <> Options.short 'c'
        <> Options.value defaultConfigFile
        <> Options.showDefault
        <> Options.metavar "FILE"
        <> Options.help "Path to configuration file"
    defaultConfigFile = "appTemplate.cfg"

-- | Parse config file.
parseConfigFile :: FilePath -> IO Config
parseConfigFile filePath = C.load [C.Required filePath] >>= parser
  where
    parser cfg = Config <$> logSectionParser cfg
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

