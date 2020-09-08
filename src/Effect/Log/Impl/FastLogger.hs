{- | Log functionalities, relying on fast-logger package. As in multithreaded programs each thread has
     its own buffer, the order of output will not be the order of occurrance, because each thread's log
     buffer flushes when its full. The result is somewhat surprising. So we set the buffer size to
     1, flushing after each log call.  -}
module Effect.Log.Impl.FastLogger
    ( HasResource(..)
    , Resource(..)
    , def
    ) where

import           Effect.Log

import qualified Control.Concurrent as C
import qualified Control.Concurrent.Async as C
import           Control.Monad (forever, void, when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.List as L
import           Data.Maybe (fromMaybe)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import           Formatting ((%))
import qualified Formatting as F
import qualified System.Log.FastLogger as FL

data Sink = Sink FL.LoggerSet !Level !Effect.Log.Destination

data Item = Item !FL.LoggerSet !Effect.Log.Destination !FL.LogStr

-- | Logging resource.
data Resource = Resource { resourceSink :: !(C.MVar (Maybe Sink))
                         , resourceTimeCache :: !(IO FL.FormattedTime)
                         , resourceChan :: !(C.Chan Item)
                         }

def :: IO Resource
def = do
    -- use time cache because getting and formatting time is expensive
    timeCache <- FL.newTimeCache FL.simpleTimeFormat

    -- open channel to synchronize write request because fast logger does not guarantee
    -- order when used multithreaded
    chan <- C.newChan

    -- sink not initialized
    sink <- C.newMVar Nothing

    -- start reading channel
    void $ C.async $ forever (C.readChan chan >>= logStr)

    return $ Resource sink timeCache chan

-- | Provide resource.
class HasResource m where
    getResource :: m Resource

-- implement logging in IO using resource
instance (Monad m, MonadIO m, HasResource m) => LogM m where
    init minLogLevel logDestination = do
        loggerSet <- liftIO $ case logDestination of
            StdOut -> FL.newStdoutLoggerSet FL.defaultBufSize
            File filePath -> FL.newFileLoggerSet FL.defaultBufSize filePath

        let sink = Sink loggerSet minLogLevel logDestination

        getResource >>= \res -> liftIO . void $
            -- swap mvar, new write requests will use new sink
            (resourceSink res `C.swapMVar` Just sink)

    log level msg = do
        Resource sink timeCache chan <- getResource
        liftIO $ C.readMVar sink >>= \case
            -- skip if logging not initialized
            Nothing -> return ()
            Just (Sink loggerSet minLogLevel dest) ->
                -- skip logging if minimum log level not reached
                when (level >= minLogLevel) $
                    -- get formatted time from cache and queue for logging
                    timeCache >>= C.writeChan chan . Item loggerSet dest . formatStr level msg

logStr :: Item -> IO ()
logStr (Item loggerSet dest str) = do
    FL.pushLogStrLn loggerSet str
    -- stdout seems not to flush automatically
    when (dest == StdOut) $ FL.flushLogStr loggerSet

-- display log level without Show instance because we want to avoid Prelude String
-- error only happens if code is inconsistent
display :: Level -> TL.Text
display = fromMaybe (Prelude.error "log level not found") . flip L.lookup levels

-- annotate message like this: [INFO] [Time] msg
formatStr :: Level -> TL.Text -> FL.FormattedTime -> FL.LogStr
formatStr level msg formattedTime = FL.toLogStr $
    F.format ("[" % F.text % "] [" % F.stext % "] " % F.text)
             (display level)
             (T.decodeUtf8 formattedTime)
             msg

