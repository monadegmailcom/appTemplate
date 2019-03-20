{- | The `Environment` contains the application's state and mockable services as
     logging, remote http server, database connection etc.
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
    guard <- GracefulShutdown.createGuard
     
    logFunction <-
            Log.makeLoggerSet mPath
        >>= Log.makeLogFunction logLevel
    return $ Environment config logFunction guard
  where
    onConfigParsingError = fail . ("Error parsing config file: " <>) . show
