{- | Log functionalities, relying on fast-logger package. As in multithreaded programs each thread has
     its own buffer, the order of output will not be the order of occurrance, because each thread's log
     buffer flushes when its full. The result is somewhat surprising. So we set the buffer size to
     1, flushing after each log call.  -}
module Effect.Log.Impl.FastLogger
    ( HasResource(..)
    , Resource(..)
    ) where

import           Effect.Log
import           Effect.Log.Init

import qualified Control.Concurrent as C
import           Control.Monad (void, when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.List as L
import           Data.Maybe (fromMaybe)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import           Formatting ((%))
import qualified Formatting as F
import qualified System.Log.FastLogger as FL

-- | Logging resource.
data Resource = Resource { resourceLoggerSet :: !FL.LoggerSet
                         , resourceMinLogLevel :: !Level
                         , resourceTimeCache :: !(IO FL.FormattedTime)
                         }

-- | Provide resource.
class HasResource m where
    getResource :: m (C.MVar Resource)

-- implement logging in IO using resource
instance (Monad m, MonadIO m, HasResource m) => LogM m where
    log level msg = getResource >>= liftIO . C.tryReadMVar >>= \case
        -- just ignore if not initialized
        Nothing -> return ()
        Just (Resource loggerSet minLogLevel timeCache) ->
            -- skip logging if minimum log level not reached
            when (level >= minLogLevel) $
                -- get formatted time from cache
                liftIO timeCache >>= liftIO . FL.pushLogStrLn loggerSet . formatStr level msg

-- initialize resource
instance (Monad m, MonadIO m, HasResource m) => InitM m where
    init minLogLevel logDestination = do
        loggerSet <- liftIO $ case logDestination of
            StdOut -> FL.newStdoutLoggerSet bufferSize
            File filePath -> FL.newFileLoggerSet bufferSize filePath
        -- use time cache because getting and formatting time is expensive
        timeCache <- liftIO $ FL.newTimeCache FL.simpleTimeFormat
        -- empty mvar if already taken
        getResource >>= liftIO . void . C.tryTakeMVar
        -- reset mvar
        getResource >>= liftIO . void . (`C.tryPutMVar` Resource loggerSet minLogLevel timeCache)

-- construct logger sets without buffering (1 byte buffer)
bufferSize :: Int
bufferSize = 1

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

