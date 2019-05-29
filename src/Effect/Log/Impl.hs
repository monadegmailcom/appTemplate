{-# OPTIONS_GHC -fno-warn-orphans #-}

{- | Log functionalities, relying on fast-logger package. As in multithreaded programs each thread has
     its own buffer, the order of output will not be the order of occurrance, because each thread's log
     buffer flushes when its full. The result is somewhat surprising. So we set the buffer size to
     1, flushing after each log call.  -}
module Effect.Log.Impl
    ( Function
    , HasLog(..)
    , makeLoggerSet
    , makeLogFunction
    ) where

import           Control.Monad (when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.List as L
import           Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as T
import           Effect.Log
import           Formatting ((%))
import qualified Formatting as F
import qualified System.Log.FastLogger as FL

{- | This logger set may be used to build a log function or to be passed to wai logging. -}
makeLoggerSet :: Maybe FilePath -> IO FL.LoggerSet
makeLoggerSet filePath = maybe FL.newStdoutLoggerSet (flip FL.newFileLoggerSet) filePath bufferSize
  where
     -- construct logger sets without buffering (1 byte buffer)
     bufferSize = 1

-- | Log function, filter entries with appropriate log level.
type Function = Level -> TL.Text -> IO ()

-- | Provide log function implementation.
class HasLog m where
    getLogFunction :: m Function

-- implement logging in IO using 'HasLog'
instance (Monad m, MonadIO m, HasLog m) => LogM m where
    log level msg = do
        logFunction <- getLogFunction
        liftIO $ logFunction level msg

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

-- | Make log function from logger set.
makeLogFunction :: Level -> FL.LoggerSet -> IO Function
makeLogFunction minLogLevel loggerSet =
    -- use time cache because getting and formatting time is expensive
    getLoggerFunction <$> FL.newTimeCache FL.simpleTimeFormat
  where
    -- skip logging if minimum log level not reached
    getLoggerFunction timeCache logLevel str = when (logLevel >= minLogLevel) $
        -- get formatted time from cache
        timeCache >>= FL.pushLogStrLn loggerSet . formatStr logLevel str
