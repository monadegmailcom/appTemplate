{- | Application's polling events. -}
module Poll
    ( pollRedis
    , pollState
    ) where

import qualified Config
import qualified Effect.Database as Database
import qualified Effect.Log as Log
import qualified Effect.State as State
import qualified Effect.Thread as Thread

import qualified Control.Exception.Safe as E
import           Control.Monad ((>=>), void)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Builder as BS
import           Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
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
pollRedis :: (E.MonadCatch m, Log.LogM m, Database.DatabaseM Config.Redis m, Thread.ThreadM m)
          => Config.Redis -> TL.Text -> m ()
pollRedis configRedis msg = inc
            >>= Log.info . F.format ("poll redis " % F.text % " " % F.text) msg
             >> Thread.delay delay
             >> Log.info (msg <> " ..done")
  where
    inc = do
        value <- maybe "Timeout" (fromMaybe "Not set")
             <$> Thread.timeout timeout (retryOnce reconnect $ Database.getByKey @Config.Redis key)
        let new = case readMaybe @Int . BS.unpack $ value of
                Nothing -> "1"
                Just i -> BL.toStrict . BS.toLazyByteString . BS.intDec $ i + 1
        void $ Thread.timeout timeout (retryOnce reconnect $ Database.setByKey key new)
        return . TL.decodeUtf8 . BL.fromStrict $ value
    timeout = Time.Units.sec 1
    delay = Time.Units.sec 1
    key = "msg"

    reconnect e = do
        Log.warning $ "Redis action failed: " <> (TL.pack . show) e <> "\nReconnect and try again"
        Database.connect configRedis

-- execute action, handle exception (if any) and retry once
retryOnce :: E.MonadCatch m => (E.SomeException -> m ()) -> m a -> m a
retryOnce handleException action = E.catchAny action (handleException >=> const action)
