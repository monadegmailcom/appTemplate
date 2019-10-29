module Tests.ConfigSpec (spec) where

import           Config (Config(..))
import qualified Config
import qualified Data.Text.IO as T
import qualified Effect.CmdLine as CmdLine
import qualified Effect.CmdLine.Impl as CmdLine ()
import qualified Effect.Log as Log
import qualified System.Environment
import           Test.Hspec

spec :: Spec
spec = context "Config" $
    context "with valid config file" $ before (return args) $
        it "succeeds parsing config file" $ flip System.Environment.withArgs $ do
            (config, _) <-     CmdLine.parseCommandLineOptions
                           >>= T.readFile . CmdLine.cmdLineConfigFile
                           >>= either error return . Config.parseIniFile
            config `shouldBe` validConfig
  where
    validConfig = Config (Config.Log Nothing Log.Info)
    args = ["-c", fixturesDir <> "valid.ini"]
    fixturesDir = "test/fixtures/"
