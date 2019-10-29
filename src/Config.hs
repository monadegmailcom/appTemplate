{- | Command line and config file parsing. -}
module Config
    ( Config(..)
    , Log(..)
    , parseIniFile
    ) where

import           Data.Bifunctor (first)
import qualified Data.CaseInsensitive as CI
import qualified Data.Ini as Ini
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Effect.Log as Log
import           Formatting ((%))
import qualified Formatting as F

-- | Global configuration.
newtype Config = Config
    { configLog :: Log
    } deriving (Eq, Show)

-- | Logging configuration.
data Log = Log
    { logFile :: !(Maybe FilePath) -- ^ log to stdout if Nothing
    , logLevel :: !Log.Level -- ^ filter messages with minimum level
    } deriving (Eq, Show)

-- | Parse ini file semantically.
parseIniFile :: T.Text -> Either String (Config, T.Text)
parseIniFile content = do
    ini <- Ini.parseIni content
        -- handle empty values as not present
    let lookupValue section key = Ini.lookupValue section key ini >>= text2Either
        -- lookup optional value as Maybe
        lookupOptional section key = case lookupValue section key of
            Left _  -> Right Nothing
            Right v -> Right $ Just v
    mPath <- lookupOptional "Log" "path"
    level <- lookupValue "Log" "level" >>= toLogLevel
    return (Config $ Log (T.unpack <$> mPath) level, Ini.printIni ini)
  where
    text2Either v = if T.null v then Left "empty value" else Right v
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

