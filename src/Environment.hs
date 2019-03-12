{- | The environment contains the application's mockable services as
     remote http server, database connection etc.
-}
module Environment
    ( Environment(..)
    , create
    ) where

import Config (Config(..))
import qualified Config

import qualified Log

-- | Global application environment
data Environment = Environment
    { envConfig :: Config
    , envLogFunction :: Log.Function
    }

instance Log.HasLog Environment where
    getLogFunction = envLogFunction

-- | Create environment, all external services are bound to "real" ones.
create :: Config -> IO Environment
create config@(Config (Config.Log mPath logLevel)) = do
    logFunction <-
            Log.makeLoggerSet mPath
        >>= Log.makeLogFunction logLevel
    return $ Environment config logFunction
