module Log
    ( Level(..)
    , Msg(..)
    , LogM
    , Resource(..)
    , stream
    , toMsg
    , logger
    , info
    , debug
    , warning
    , error
    , flush
    ) where

import qualified Effect.Filesystem as FS
import qualified Effect.Concurrent.Stream as Stream
import qualified Effect.Concurrent.STM as STM
import qualified Effect.Concurrent.Thread as Thread
import qualified Effect.Time as Time

import           Control.Monad (when)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Time.LocalTime as LocalTime
import           Formatting ((%))
import qualified Formatting as F
import qualified Formatting.Time as F
import           Prelude hiding (error, log)
import qualified Streamly as S

-- | Ordered log levels.
data Level = Debug | Info | Warning | Error deriving (Eq, Ord, Show, Enum)

-- | Log message or flushing.
data Msg = Flush
         | Msg LocalTime.LocalTime Thread.ThreadId Level TL.Text
    deriving (Eq, Show)

-- | Contraints for logging.
type LogM m = (Thread.ThreadM m, STM.STM m, Time.TimeM m)

-- | Log resource for logging client side.
data Resource m =
    Resource { resourceMsgChan :: STM.TChan m Msg
             , resourceMinLevelRelay :: STM.TVar m Level
             }

-- | Enrich log message str.
toMsg :: (Thread.ThreadM m, Time.TimeM m) => Level -> TL.Text -> m Msg
toMsg level str = do
    threadId <- Thread.myThreadId
    time <- LocalTime.zonedTimeToLocalTime <$> Time.getZonedTime
    return $ Msg time threadId level str

-- check log level in write message to channel
log :: LogM m => Level -> Resource m -> TL.Text -> m ()
log level (Resource msgChan minLevelRelay) str = do
    minLevel <- STM.readTVar' minLevelRelay
    when (level >= minLevel) $
        toMsg level str >>= STM.writeTChan' msgChan

-- | Force flushing.
flush :: LogM m => Resource m -> m ()
flush = (`STM.writeTChan'` Flush) . resourceMsgChan

-- | Log according level.
debug, info, warning, error :: LogM m => Resource m -> TL.Text -> m ()
debug = log Debug
info = log Info
warning = log Warning
error = log Error

-- | Stream of log messages. The last message before blocking on an empty
--   channel is a flush.
stream :: (S.IsStream t, Stream.StreamM m, STM.STM m)
       => STM.TChan m Msg
       -> t m Msg
stream msgChan = Stream.unfoldrM (fmap Just . f) False
  where
    -- block after flushing, otherwise spin
    f block =
        if block
        then (, False) <$> STM.readTChan' msgChan
        -- insert flush before block if no msg available
        else maybe (Log.Flush, True) (,False) <$> STM.tryReadTChan' msgChan

-- | Log (synchronously) to file handle.
logger :: FS.FilesystemM m h => h -> Msg -> m ()
logger handle = \case
    (Msg time threadId level str) ->
        let txt =
              F.format ("[" % F.datetime % "] [" % F.shown % "] [" % F.shown % "] " % F.text % "\n")
                       time level threadId str
        in FS.hPutStr handle . TL.encodeUtf8 $ txt
    Flush -> FS.hFlush handle

