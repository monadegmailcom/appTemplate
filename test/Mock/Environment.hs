{- | Mock services like logging, database connection etc.
-}
module Mock.Environment
    ( create ) where

import           Config (Config(..))
import qualified Control.Concurrent as C
import           Environment (Environment(..))
import qualified GracefulShutdown
import qualified Log
import qualified System.Log.FastLogger as FL

-- | Mocked application environment.
create :: C.MVar [(Log.Level, FL.LogStr)] -> Config -> IO Environment
create logSink config = do
    (transactions, shutdownFlag) <- GracefulShutdown.createSyncPrimitives
    return $ Environment config logFunction transactions shutdownFlag
  where
    logFunction level str = C.modifyMVar_ logSink (return . ((level, str) :))

