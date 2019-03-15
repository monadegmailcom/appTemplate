{- | The `Environment` contains the application's state and mockable services as
     logging, remote http server, database connection etc.
-}
module Environment
    ( Environment(..)
    , create
    ) where

import qualified Control.Concurrent as C
import           Config (Config(..))
import           Control.Exception.Safe (handleAny)
import qualified Config
import qualified Log

-- | Global application environment
--   Includes a "busy" variable, if set, the application should not be interrupted, if empty
--   it indicates an interruptable "idle" state. Likewise a set "shutdown" variable may prevent
--   the application to start new tasks.
--   It is used by the signal handlers to process a gracefull shutdown.
data Environment = Environment
    { envConfig :: !Config
    , envLogFunction :: !Log.Function
    , envBusy :: !(C.MVar ()) -- ^ indicate application's busy state
    , envShutdown :: !(C.MVar ()) -- ^ indicate application's shutdown state
    }

instance Log.HasLog Environment where
    getLogFunction = envLogFunction

-- | Create environment with application's 'Settings', all external services are bound to "real" ones.
create :: IO Environment
create = do
    config@(Config (Config.Log mPath logLevel)) <-
            Config.parseCommandLineOptions
        >>= handleAny onConfigParsingError . Config.parseConfigFile . Config.cmdLineConfigFile
    busyFlag <- C.newMVar ()
    shutdownFlag <- C.newEmptyMVar
    logFunction <-
            Log.makeLoggerSet mPath
        >>= Log.makeLogFunction logLevel
    return $ Environment config logFunction busyFlag shutdownFlag
  where
    onConfigParsingError = fail . ("Error parsing config file: " <>) . show
