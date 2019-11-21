{- | Internal implementation exported for unit tests. -}
module Config.Internal
    ( Config(..)
    , Log(..)
    , Redis(..)
    , parseFromIni
    ) where

import           Data.Bifunctor (first)
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text.Read as T.R
import qualified Data.Ini as Ini
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Database.Redis as Redis
import qualified Effect.Log as Log
import           Formatting ((%))
import qualified Formatting as F

-- | Global configuration.
data Config = Config
    { configLog :: !Log
    , configRedis :: !Redis
    } deriving (Eq, Show)

-- | Logging configuration.
data Log = Log
    { logFile :: !Log.Destination -- ^ log to stdout or file
    , logLevel :: !Log.Level -- ^ filter messages with minimum level
    } deriving (Eq, Show)

-- | Redis configuration.
newtype Redis = Redis
    { redisConnectInfo :: Redis.ConnectInfo
    } deriving (Eq, Show)

-- compare modulo tls
instance Eq Redis.ConnectInfo
    where
        (Redis.ConnInfo _lhs1 _lhs2 _lhs3 _lhs4 _lhs5 _lhs6 _lhs7 Nothing) ==
          (Redis.ConnInfo _rhs1 _rhs2 _rhs3 _rhs4 _rhs5 _rhs6 _rhs7 Nothing)
            =    _lhs1 == _rhs1
              && _lhs2 == _rhs2
              && _lhs3 == _rhs3
              && _lhs4 == _rhs4
              && _lhs5 == _rhs5
              && _lhs6 == _rhs6
              && _lhs7 == _rhs7
        _ == _ = False

-- | Parse ini file semantically.
parseFromIni :: Ini.Ini -> Either String Config
parseFromIni ini = do
    logger <- do
        let section = "Log"
            destination = case T.unpack <$> lookupOptional section "path" of
                Nothing -> Log.StdOut
                Just path -> Log.File path
        level <- lookupMandatory section "level" >>= toLogLevel
        return $ Log destination level
    redis <- do
        let section = "Redis"
        mConnectTimeout <- maybe (Right Nothing)
                                 (fmap (Just . fromRational) . total T.R.rational)
                                 (lookupOptional section "connectTimeout")
        connectInfo <- lookupMandatory section "url"
            >>= Redis.parseConnectInfo . T.unpack
            >>= \ci -> Right $ ci { Redis.connectTimeout = mConnectTimeout }
        Right . Redis $ connectInfo
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
