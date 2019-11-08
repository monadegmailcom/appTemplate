{- | Internal implementation for unit tests. -}
module Config.Internal
    ( Config(..)
    , Log(..)
    , Redis(..)
    , parseFromIni
    ) where

import           Data.Bifunctor (first)
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text.Encoding as T.E
import qualified Data.Text.Read as T.R
import qualified Data.Ini as Ini
import qualified Data.List as L
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Time.Clock as Clock
import qualified Database.Redis as Redis
import qualified Effect.Log as Log
import           Formatting ((%))
import qualified Formatting as F
import qualified Network.Socket as Socket

-- | Global configuration.
data Config = Config
    { configLog :: !Log
    , configRedis :: !Redis
    } deriving (Eq, Show)

-- | Redis configuration.
data Redis = Redis
    { redisHost           :: !Redis.HostName
    , redisPort           :: !Socket.PortNumber
    , redisAuth           :: !(Maybe BS.ByteString)
    , redisConnectTimeout :: !(Maybe Clock.NominalDiffTime)
    } deriving (Eq, Show)

-- | Logging configuration.
data Log = Log
    { logFile :: !(Maybe FilePath) -- ^ log to stdout if Nothing
    , logLevel :: !Log.Level -- ^ filter messages with minimum level
    } deriving (Eq, Show)

-- | Parse ini file semantically.
parseFromIni :: Ini.Ini -> Either String Config
parseFromIni ini = do
    logger <- do
        let section = "Log"
        let mPath = T.unpack <$> lookupOptional section "path"
        level <- lookupMandatory section "level" >>= toLogLevel
        return $ Log mPath level
    redis <- do
        let section = "Redis"
        host <- T.unpack <$> lookupMandatory section "host"
        port <- lookupMandatory section "port" >>= fmap fromInteger . total T.R.decimal
        let mAuth = T.E.encodeUtf8 <$> lookupOptional section "auth"
        mConnectTimeout <- maybe (Right Nothing)
                                 (fmap (Just . fromRational) . total T.R.rational)
                                 (lookupOptional section "connectTimeout")
        return $ Redis host port mAuth mConnectTimeout
    Right $ Config logger redis
  where
    lookupOptional :: T.Text -> T.Text -> Maybe T.Text
    lookupOptional section key = (HashMap.lookup section . Ini.unIni) ini >>= HashMap.lookup key
    lookupMandatory :: T.Text -> T.Text -> Either String T.Text
    lookupMandatory section key =
        maybe (Left . T.unpack $ "Couldn't find key: " <> section <> "." <> key)
              Right
              $ lookupOptional section key
    -- partial reading (non-empty left string in snd part of pair) should result in an error
    total reader str = reader str >>= \case
        (v, "") -> Right v
        x -> Left . show $ x
    maybe2Either e = maybe (Left e) Right
    swap (a,b) = (b,a)
    -- translate case insensitive string to log level
    toLogLevel str =
        let logLevels = map (first (CI.mk . TL.toStrict) . swap) Log.levels
            levelsStr = TL.intercalate ", " . map snd $ Log.levels
            errorMsg = TL.unpack $ F.format
                ("Invalid log level '" % F.stext % "', choose one of " % F.text)
                str
                levelsStr
        in maybe2Either errorMsg . L.lookup (CI.mk str) $ logLevels
