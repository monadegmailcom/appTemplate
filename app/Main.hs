module Main where

import qualified App
import qualified Config
import qualified Control.Concurrent as C
import           Control.Exception.Safe (handleAny)
import qualified Log
import           MultiReader ((<:>))
import qualified MultiReader as MR
import qualified State

-- entry point of process, start application with production environment
main :: IO ()
main = do
    -- parse command line and parse config file accordingly
    config <- Config.parseCommandLineOptions
          >>= handleAny onConfigParsingError
            . Config.parseConfigFile
            . Config.cmdLineConfigFile
    env <- let Config.Log mPath logLevel = Config.configLog config
               makeLogFunction = Log.makeLoggerSet mPath >>= Log.makeLogFunction logLevel
           in MR.makeList <$> makeLogFunction
    -- termination signals should be thrown as async exception to main thread
    C.myThreadId >>= MR.runReader env . App.installSignalHandlers
    state <- State.defaultState
    MR.runReader (config <:> state <:> env) App.run
  where
    -- do not start if config file parsing fails
    onConfigParsingError = fail . ("Error parsing config file: " <>) . show
