{- | Mock services like logging, database connection etc. -}
module Mock.Environment
    ( createLogFunction ) where

import qualified Control.Concurrent as C
import qualified Log
import qualified System.Log.FastLogger as FL

-- | Mocked application environment.
createLogFunction :: C.MVar [(Log.Level, FL.LogStr)] -> Log.Function
createLogFunction logSink = logFunction
  where
    logFunction level str = C.modifyMVar_ logSink (return . ((level, str) :))

