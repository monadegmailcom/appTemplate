{- | Log functionalities relying on fast-logger package. As in multithreaded programs each thread has
     its own buffer, the order of output will not be the order of occurrance, because each thread's log
     buffer flushes when its full. This results in surprising behaviour. So we set the buffer size to
     1, flushing on each log call.
-}
module Log
    ( Function
    , HasLog(..)
    , Level(..)
    , Log.debug
    , Log.error
    , Log.info
    , Log.warning
    , Log.levels
    , makeLogFunction
    , makeLoggerSet
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, asks)

import qualified Data.List as L
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

import qualified System.Log.FastLogger as FL

{- | Make logger set without buffering (1 byte buffer). This logger set may be used to build a log
     function or to be passed to wai logging. -}
makeLoggerSet :: Maybe FilePath -> IO FL.LoggerSet
makeLoggerSet filePath = case filePath of
    -- construct logger sets without buffering (1 byte buffer)
    Nothing   -> FL.newStdoutLoggerSet 1
    Just path -> FL.newFileLoggerSet 1 path

-- | Ordered log level.
data Level = Debug | Info | Warning | Error deriving (Eq, Ord, Show)

-- | Available log levels.
levels :: [(Level, T.Text)]
levels =
    [ (Debug, "Debug")
    , (Info, "Info")
    , (Warning, "Warning")
    , (Error, "Error")
    ]

-- display log level without Show instance because we want to avoid prelude String
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
    return $ loggerFunction timeCache
  where
    loggerFunction timeCache logLevel str
        | logLevel < minLogLevel = return () -- skip logging if minimum log level not reached
        | otherwise = do
              formattedTime <- FL.toLogStr <$> timeCache
              -- annotate like this: [INFO] [Time] str
              let str' = foldl annotateWith str [formattedTime, FL.toLogStr . display $ logLevel]
              FL.pushLogStrLn loggerSet str'
    annotateWith str annotation = "[" <> annotation <> "] " <> str

type Constraint s env m = (FL.ToLogStr s, MonadIO m, MonadReader env m, HasLog env)

-- | Typeclass for logging functionality.
class HasLog a where
    getLogFunction :: a -> Log.Function

-- helper function for debug, info, warning and error
genericLog :: Constraint s env m => Log.Level -> s -> m ()
genericLog minLogLevel str = do
    logFunction <- asks getLogFunction
    liftIO . logFunction minLogLevel . FL.toLogStr $ str

-- | Log with log level Debug.
debug :: Constraint s env m => s -> m ()
debug = genericLog Debug

-- | Log with log level Info.
info :: Constraint s env m => s -> m ()
info = genericLog Info

-- | Log with log level Warning.
warning :: Constraint s env m => s -> m ()
warning = genericLog Warning

-- | Log with log level Error.
error :: Constraint s env m => s -> m ()
error = genericLog Error
