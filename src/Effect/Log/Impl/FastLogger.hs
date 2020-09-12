{- | Log functionalities, relying on fast-logger package. -}
module Effect.Log.Impl.FastLogger
    ( HasResource(..)
    , Resource(..)
    , def
    , flush
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

data Sink = Sink FL.LoggerSet !Level

data Item = Item !FL.LoggerSet !FL.LogStr

-- | Logging resource.
data Resource = Resource { resourceSink :: !(C.MVar (Maybe Sink))
                         , resourceTimeCache :: !(IO FL.FormattedTime)
                         , resourceChan :: !(C.Chan Item)
                         , resourceItem :: !(C.MVar Item)
                         }

flush :: (MonadIO m, HasResource m) => m ()
flush = do
    Resource sink _ chan _ <- getResource
    liftIO $ do
        C.getChanContents chan >>= mapM_ logStr
        C.readMVar sink >>= \case
            Nothing -> return ()
            Just (Sink loggerSink _) -> FL.flushLogStr loggerSink

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

    item <- C.newEmptyMVar

    return $ Resource sink timeCache chan item

-- | Provide resource.
class HasResource m where
    getResource :: m Resource

-- implement logging in IO using resource
instance (Monad m, MonadIO m, HasResource m) => LogM m where
    init minLogLevel logDestination = do
        loggerSet <- liftIO $ case logDestination of
            StdOut -> FL.newStdoutLoggerSet FL.defaultBufSize
            File filePath -> FL.newFileLoggerSet FL.defaultBufSize filePath

        void $ getResource >>= liftIO .
            -- swap mvar, new write requests will use new sink
            (`C.swapMVar` Just (Sink loggerSet minLogLevel)) . resourceSink

    log level msg = do
        Resource sink timeCache chan _ <- getResource
        liftIO $ C.readMVar sink >>= \case
            -- skip if logging not initialized
            Nothing -> return ()
            Just (Sink loggerSet minLogLevel) ->
                -- skip logging if minimum log level not reached
                when (level >= minLogLevel) $
                    -- get formatted time from cache and queue for logging
                    timeCache >>= C.writeChan chan . Item loggerSet . formatStr level msg

logStr :: Item -> IO ()
logStr (Item loggerSet str) = FL.pushLogStrLn loggerSet str

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

