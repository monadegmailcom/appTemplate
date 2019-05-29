{- | Mock services like logging, database connection etc. -}
module Mock.Environment
    ( createLogFunction ) where

import qualified Control.Concurrent as C
import qualified Data.Text.Lazy as TL
import qualified Effect.Log as Log
import qualified Effect.Log.Impl as Log

-- | Mocked application environment.
createLogFunction :: C.MVar [(Log.Level, TL.Text)] -> Log.Function
createLogFunction logSink = logFunction
  where
    logFunction level str = C.modifyMVar_ logSink (return . ((level, str) :))

