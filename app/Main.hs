module Main where

import qualified App
import qualified Config
import qualified Control.Concurrent as C
import           Control.Exception.Safe (handleAny)
import qualified Log
import           MultiReader ((<:>))
import qualified MultiReader as MR

-- entry point of process, start application with production environment
main :: IO ()
main = do
    -- build environment for application's 'run' function
    config <- buildConfig
    logFunction <- buildLogFunction . Config.configLog $ config
    let env = logFunction <:> config <:> MR.nil
   
    -- termination signals should be thrown as async exception to main thread
    C.myThreadId >>= MR.runReader env . App.installSignalHandlers
    MR.runReader env App.run
  where
    -- parse command line and parse config file accordingly
    buildConfig =
            Config.parseCommandLineOptions
        >>= handleAny onConfigParsingError . Config.parseConfigFile . Config.cmdLineConfigFile
    buildLogFunction (Config.Log mPath logLevel) =
        Log.makeLoggerSet mPath >>= Log.makeLogFunction logLevel
    -- do not start if config file parsing fails
    onConfigParsingError = fail . ("Error parsing config file: " <>) . show
