module Main where

import qualified App

import qualified Config
import Control.Exception.Safe (handleAny)
import Control.Monad.Reader (runReaderT)

import qualified Environment

import qualified Settings

-- entry point of process
main :: IO ()
main =
        Config.parseCommandLineOptions
    >>= handleAny onConfigParsingError . Config.parseConfigFile . Config.cmdLineConfigFile
    >>= Environment.create
    >>= Settings.create
    >>= runReaderT App.run
  where
    onConfigParsingError = fail . ("Error parsing config file: " <>) . show
