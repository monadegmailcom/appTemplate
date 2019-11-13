{- | Application's polling events. -}
module Poll
    ( pollRedis
    , pollState
    ) where

import           Control.Monad (void)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Effect.Database as Database
import qualified Effect.Log as Log
import qualified Effect.State as State
import qualified Effect.Thread as Thread
import           Formatting ((%))
import qualified Formatting as F
import           Text.Read (readMaybe)
import qualified Time.Units

-- | Example async processing with state
pollState :: (Log.LogM m, State.StateM m, Thread.ThreadM m) => TL.Text -> m ()
pollState msg = inc
            >>= Log.info . F.format ("poll state " % F.text % " " % F.int) msg
             >> Thread.delay delay
             >> Log.info (msg <> " ..done")
  where
    inc = do
        prev <- State.getCount
        State.incCount
        return prev
    delay = Time.Units.sec 1

-- | Example async processing with redis
pollRedis :: (Log.LogM m, Database.DatabaseM m, Thread.ThreadM m) => TL.Text -> m ()
pollRedis msg = inc
            >>= Log.info . F.format ("poll redis " % F.text % " " % F.text) msg
             >> Thread.delay delay
             >> Log.info (msg <> " ..done")
  where
    inc = do
        value <- Thread.timeout timeout (Database.getByKey key) >>= \case
            Nothing -> return "Timeout"
            Just Nothing -> return "Not set"
            Just (Just v) -> return v
        let new = case readMaybe @Int . BS.unpack $ value of
                Nothing -> "1"
                Just i -> BL.toStrict . BS.toLazyByteString . BS.intDec $ i + 1
        void $ Thread.timeout timeout (Database.setByKey key new)
        return . TL.decodeUtf8 . BL.fromStrict $ value
    timeout = Time.Units.sec 1
    delay = Time.Units.sec 1
    key = "msg"
