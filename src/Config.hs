{- | Command line and config file parsing. -}
module Config
    ( CommandLineOptions(..)
    , Config(..)
    , Log(..)
    , parseCommandLineOptions
    , parseIniFile
    ) where

import           Data.Bifunctor (first)
import qualified Data.CaseInsensitive as CI
import qualified Data.Ini as Ini
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Version as Version
import qualified Effect.Log as Log
import           Formatting ((%))
import qualified Formatting as F
import qualified Paths_appTemplate as Paths
import           System.Console.CmdArgs ((&=))
import qualified System.Console.CmdArgs as CA
import qualified System.Environment as Environment

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
parseCommandLineOptions = do
    procName <- head <$> Environment.getArgs
    CA.cmdArgs $ getCommandLineOptions procName
  where
    getCommandLineOptions procName = CommandLineOptions
        { cmdLineConfigFile
            = defaultConfigFile &= CA.typFile
           &= CA.explicit &= CA.name "c" &= CA.name "config"
           &= CA.help "Path to configuration file" }
      &= CA.summary (procName <> " " <> version)

    version = Version.showVersion Paths.version
    defaultConfigFile = "appTemplate.ini"

-- | Parse ini file semantically.
parseIniFile :: Ini.Ini -> Either String Config
parseIniFile ini = do
    mPath <- lookupOptional "Log" "path"
    level <- lookupValue "Log" "level" >>= toLogLevel
    return $ Config $ Log (T.unpack <$> mPath) level
  where
    -- handle empty values as not present
    lookupValue section key =
        let text2Either v = if T.null v then Left "empty value" else Right v
        in Ini.lookupValue section key ini >>= text2Either
    -- lookup optional value as Maybe
    lookupOptional section key = case lookupValue section key of
        Left _  -> Right Nothing
        Right v -> Right $ Just v
    -- translate case insensitive string to log level
    toLogLevel str =
        let logLevels = map (first (CI.mk . TL.toStrict) . swap) Log.levels
            swap (a,b) = (b,a)
            levelsStr = TL.intercalate ", " . map snd $ Log.levels
            errorMsg = TL.unpack $ F.format
                ("Invalid log level '" % F.stext % "', choose one of " % F.text)
                str
                levelsStr
            maybe2Either e = maybe (Left e) Right
        in maybe2Either errorMsg . L.lookup (CI.mk str) $ logLevels

