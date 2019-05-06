module Main where

import qualified App
import qualified Config
import qualified Control.Concurrent as C
import           Control.Exception.Safe (handleAny)
import qualified Log
import qualified State

-- entry point of process, start application with production environment
main :: IO ()
main = do
    -- parse command line and parse config file accordingly
    config <- Config.parseCommandLineOptions
          >>= handleAny onConfigParsingError
            . Config.parseConfigFile
            . Config.cmdLineConfigFile
    logger <- let Config.Log mPath logLevel = Config.configLog config
              in Log.makeLoggerSet mPath >>= Log.makeLogFunction logLevel
    -- termination signals should be thrown as async exception to main thread
    C.myThreadId >>= App.installSignalHandlers logger
    state <- State.defaultState
    -- run the app
    App.run logger config state
  where
    -- do not start if config file parsing fails
    onConfigParsingError = fail . ("Error parsing config file: " <>) . show
