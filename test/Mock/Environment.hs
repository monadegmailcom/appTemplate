{- | Mock services like logging, database connection etc.
-}
module Mock.Environment
    ( create ) where

import           Config (Config(..))
import qualified Control.Concurrent as C
import           Environment (Environment(..))
import qualified Log
import qualified System.Log.FastLogger as FL

-- | Mocked application environment.
create :: C.MVar [(Log.Level, FL.LogStr)] -> Config -> IO Environment
create logSink config = do
    busyFlag <- C.newMVar () -- before initialization is done, we are busy
    Environment config logFunction busyFlag <$> C.newEmptyMVar
  where
    logFunction level str = C.modifyMVar_ logSink (return . ((level, str) :))

