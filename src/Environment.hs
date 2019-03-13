{- | The environment contains the application's 'Settings' and mockable services as
     logging, remote http server, database connection etc.
-}
module Environment
    ( Environment(..)
    , create
    ) where

import           Config (Config(..))
import           Control.Exception.Safe (handleAny)
import qualified Config
import qualified Log
import           Settings (Settings)
import qualified Settings

-- | Global application environment
data Environment = Environment
    { envConfig :: Config
    , envSettings :: Settings
    , envLogFunction :: Log.Function
    }

instance Log.HasLog Environment where
    getLogFunction = envLogFunction

-- | Create environment with application's 'Settings', all external services are bound to "real" ones.
create :: IO Environment
create = do
    config@(Config (Config.Log mPath logLevel)) <-
            Config.parseCommandLineOptions
        >>= handleAny onConfigParsingError . Config.parseConfigFile . Config.cmdLineConfigFile
    settings <- Settings.create
    logFunction <-
            Log.makeLoggerSet mPath
        >>= Log.makeLogFunction logLevel
    return $ Environment config settings logFunction
  where
    onConfigParsingError = fail . ("Error parsing config file: " <>) . show
