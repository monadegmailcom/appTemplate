module Main where

import qualified App
import qualified Config
import qualified Control.Concurrent as C
import qualified Data.Configurator.Export as Configurator
import           Formatting ((%))
import qualified Formatting as F
import qualified Log
import qualified State

-- entry point of process, start application with production environment
main :: IO ()
main = do
    -- parse command line and read config file accordingly
    cfg <- Config.parseCommandLineOptions >>= Config.readConfigFile . Config.cmdLineConfigFile

    -- parse configuration and build logger
    config <- Config.parseConfigFile cfg
    logger <- let Config.Log mPath logLevel = Config.configLog config
              in Log.makeLoggerSet mPath >>= Log.makeLogFunction logLevel

    -- log current (read-only) configuration
    Configurator.renderConf cfg >>= logger Log.Info . F.format ("Initial configuration\n" % F.string)

    -- termination signals should be thrown as async exception to main thread
    C.myThreadId >>= App.installSignalHandlers logger
    state <- State.defaultState

    -- run the app
    App.run logger config state

