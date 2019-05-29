module Tests.ConfigSpec (spec) where

import           Config (Config(..))
import qualified Config
import qualified Data.Ini as Ini
import qualified Data.Text.IO as T
import qualified Effect.Log as Log
import qualified System.Environment

import Test.Hspec

spec :: Spec
spec = context "Config" $
    context "with valid config file" $ before (return args) $
        it "succeeds parsing config file" $ flip System.Environment.withArgs $
            (     Config.parseCommandLineOptions
              >>= T.readFile . Config.cmdLineConfigFile
              >>= either error return . Ini.parseIni
              >>= either error return . Config.parseIniFile) `shouldReturn` validConfig
  where
    validConfig = Config (Config.Log Nothing Log.Info)
    args = ["-c", fixturesDir <> "valid.ini"]
    fixturesDir = "test/fixtures/"
