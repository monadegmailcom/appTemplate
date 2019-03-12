module Tests.LogSpec (spec) where

import qualified Log

import qualified System.IO.Silently as Silently

import Test.Hspec 

spec :: Spec
spec = context "Log" $
    context "with log level Info" $ before (getLogFunction Log.Info) $ do
        it "includes info messages" $ \logFunction ->
            Silently.capture_ (logFunction Log.Info "hi") >>= (`shouldContain` "hi")
        it "includes warning messages" $ \logFunction ->
            Silently.capture_ (logFunction Log.Warning "hi") >>= (`shouldContain` "hi")
        it "excludes debug messages" $ \logFunction ->
            Silently.capture_ (logFunction Log.Debug "hi") >>= (`shouldBe` "")
  where
    getLogFunction logLevel = Log.makeLoggerSet Nothing >>= Log.makeLogFunction logLevel
