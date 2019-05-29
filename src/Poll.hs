{- | Application's polling events. -}
module Poll
    ( poll ) where

import qualified Data.Text.Lazy as TL
import qualified Effect.Log as Log
import qualified Effect.State as State
import qualified Effect.Thread as Thread
import           Formatting ((%))
import qualified Formatting as F

-- | Example async processing
poll :: (Log.LogM m, State.StateM m, Thread.ThreadM m) => TL.Text -> m ()
poll msg = State.incCount
        >> State.getCount
       >>= Log.info . F.format ("poll " % F.text % " " % F.int) msg
        >> Thread.delay 1000000 -- wait 1 sec
        >> Log.info (msg <> " ..done")

