module Main where

import qualified App
import App.Impl ()

-- application entry point
main :: IO ()
main = App.app

