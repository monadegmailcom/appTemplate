{- | The `Environment` addresses following requirements:

     - access and modify the application's state
     - mockable services like
       - logging
       - other services like remote http server, database access, etc
     - a 'Guard' for graceful shutdown
     - access global configuration read at startup
-}
module Environment
    ( Environment(..)
    , create
    ) where

import qualified Control.Concurrent.STM as C
import           Config (Config(..))
import           Control.Exception.Safe (handleAny)
import qualified Config
import qualified GracefulShutdown
import qualified Log

-- | Global application environment
--   Includes a registry, with IO actions not be interrupted by shutdown.
--   A 'Guard' is used by the signal handlers to process a graceful shutdown.
data Environment = Environment
    { envConfig :: !Config -- ^ Configuration.
    , envLogFunction :: !Log.Function -- ^ Log function.
    , envGuard :: !(C.TVar GracefulShutdown.Guard)
    }

instance Log.HasLog Environment where
    getLogFunction = envLogFunction

instance GracefulShutdown.HasGracefulShutdown Environment where
    getGuard = envGuard

-- | Create environment with application's 'Settings', all external services are bound to "real" ones.
create :: IO Environment
create = do
    config@(Config (Config.Log mPath logLevel)) <-
            Config.parseCommandLineOptions
        >>= handleAny onConfigParsingError . Config.parseConfigFile . Config.cmdLineConfigFile
    logFunction <-
            Log.makeLoggerSet mPath
        >>= Log.makeLogFunction logLevel
    Environment config logFunction <$> GracefulShutdown.createGuard
  where
    onConfigParsingError = fail . ("Error parsing config file: " <>) . show
