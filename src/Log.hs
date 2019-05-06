{- | Log functionalities, relying on fast-logger package. As in multithreaded programs each thread has
     its own buffer, the order of output will not be the order of occurrance, because each thread's log
     buffer flushes when its full. The result is somewhat surprising. So we set the buffer size to
     1, flushing after each log call.  -}
module Log
    ( Function
    , Level(..)
    , Log.debug
    , Log.error
    , Log.info
    , Log.levels
    , Log.warning
    , makeLogFunction
    , makeLoggerSet
    ) where

import qualified Data.List as L
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified System.Log.FastLogger as FL

{- | Make logger set without buffering (1 byte buffer). This logger set may be used to build a log
     function or to be passed to wai logging. -}
makeLoggerSet :: Maybe FilePath -> IO FL.LoggerSet
makeLoggerSet filePath = case filePath of
    -- construct logger sets without buffering (1 byte buffer)
    Nothing -> FL.newStdoutLoggerSet 1
    Just path -> FL.newFileLoggerSet 1 path

-- | Ordered log levels.
data Level = Debug | Info | Warning | Error deriving (Eq, Ord, Show)

-- | Available log levels with serialization.
levels :: [(Level, T.Text)]
levels =
    [ (Debug, "Debug")
    , (Info, "Info")
    , (Warning, "Warning")
    , (Error, "Error")
    ]

-- display log level without Show instance because we want to avoid Prelude String
-- error only happens if code is inconsistent
display :: Level -> T.Text
display = fromMaybe (Prelude.error "log level not found") . flip L.lookup levels

-- | Log function, filter entries with appropriate log level.
type Function = Level -> FL.LogStr -> IO ()

-- | Make log function from logger set.
makeLogFunction :: Log.Level -> FL.LoggerSet -> IO Log.Function
makeLogFunction minLogLevel loggerSet = do
    -- use time cache because getting and formatting time is expensive
    timeCache <- FL.newTimeCache FL.simpleTimeFormat
    return $ getLoggerFunction timeCache
  where
    getLoggerFunction timeCache logLevel str
        -- skip logging if minimum log level not reached
        | logLevel < minLogLevel = return ()
        | otherwise = do
              -- get formatted time from cache
              timeStr <- FL.toLogStr <$> timeCache
              -- annotate string like this: [INFO] [Time] str
              let str' = foldl annotateWith str [timeStr, levelStr]
                  levelStr = FL.toLogStr . display $ logLevel
              FL.pushLogStrLn loggerSet str'
    annotateWith str annotation = "[" <> annotation <> "] " <> str

-- helper function for debug, info, warning and error
genericLog :: Log.Level -> Function -> T.Text -> IO ()
genericLog minLogLevel logFunction = logFunction minLogLevel . FL.toLogStr

-- | Log with log level Debug.
debug :: Function -> T.Text -> IO ()
debug = genericLog Debug

-- | Log with log level Info.
info :: Function -> T.Text -> IO ()
info = genericLog Info

-- | Log with log level Warning.
warning :: Function -> T.Text -> IO ()
warning = genericLog Warning

-- | Log with log level Error.
error :: Function -> T.Text -> IO ()
error = genericLog Error
