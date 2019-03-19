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
import           Data.Unique (Unique)
import qualified GracefulShutdown
import qualified Log

-- | Global application environment
--   Includes a registry, with IO actions not be interrupted by shutdown.
--   A "shutdown" flag is used by the signal handlers to process a graceful shutdown.
data Environment = Environment
    { envConfig :: !Config -- ^ Configuration.
    , envLogFunction :: !Log.Function -- ^ Log function.
    , envRegistry :: !(C.TVar [Unique])
          -- ^ Application's registry of IO actions not to be interrupted.
    , envShutdown :: !(C.TVar Bool) -- ^ Indicate application's shutdown state.
    }

instance Log.HasLog Environment where
    getLogFunction = envLogFunction

instance GracefulShutdown.HasGracefulShutdown Environment where
    getShutdown = envShutdown
    getRegistry = envRegistry

-- | Create environment with application's 'Settings', all external services are bound to "real" ones.
create :: IO Environment
create = do
    config@(Config (Config.Log mPath logLevel)) <-
            Config.parseCommandLineOptions
        >>= handleAny onConfigParsingError . Config.parseConfigFile . Config.cmdLineConfigFile
    (registry, shutdownFlag) <- GracefulShutdown.createSyncPrimitives
     
    logFunction <-
            Log.makeLoggerSet mPath
        >>= Log.makeLogFunction logLevel
    return $ Environment config logFunction registry shutdownFlag
  where
    onConfigParsingError = fail . ("Error parsing config file: " <>) . show
