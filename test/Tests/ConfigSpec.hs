module Tests.ConfigSpec (spec) where

import           Config (Config(..))
import qualified Config
import qualified Log
import qualified System.Environment

import Test.Hspec

spec :: Spec
spec = context "Config" $
    context "with valid config file" $ before (return args) $
        it "succeeds parsing config file" $ flip System.Environment.withArgs $ do
            Config.CommandLineOptions configFile <- Config.parseCommandLineOptions
            Config.parseConfigFile configFile `shouldReturn` validConfig
  where
    validConfig = Config (Config.Log Nothing Log.Info)
    args = ["-c", fixturesDir <> "valid.cfg"]
    fixturesDir = "test/fixtures/"
