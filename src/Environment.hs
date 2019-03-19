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
--   Includes a "transactions" container, with transactions not be interrupted by shutdown.
--   A "shutdown" flag is used by the signal handlers to process a gracefull shutdown.
data Environment = Environment
    { envConfig :: !Config
    , envLogFunction :: !Log.Function
    , envTransactions :: !(C.TVar [Unique])
          -- ^ application's transaction not to be interrupted
    , envShutdown :: !(C.TVar Bool) -- ^ indicate application's shutdown state
    }

instance Log.HasLog Environment where
    getLogFunction = envLogFunction

instance GracefulShutdown.HasGracefulShutdown Environment where
    getShutdown = envShutdown
    getTransactions = envTransactions

-- | Create environment with application's 'Settings', all external services are bound to "real" ones.
create :: IO Environment
create = do
    config@(Config (Config.Log mPath logLevel)) <-
            Config.parseCommandLineOptions
        >>= handleAny onConfigParsingError . Config.parseConfigFile . Config.cmdLineConfigFile
    (transactions, shutdownFlag) <- GracefulShutdown.createSyncPrimitives
     
    logFunction <-
            Log.makeLoggerSet mPath
        >>= Log.makeLogFunction logLevel
    return $ Environment config logFunction transactions shutdownFlag
  where
    onConfigParsingError = fail . ("Error parsing config file: " <>) . show
