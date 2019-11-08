module Tests.ConfigSpec (spec) where

import qualified Config
import qualified Config.Internal
import           Data.Either (isRight)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Ini as Ini
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Effect.CmdLine as CmdLine
import qualified Effect.CmdLine.Impl as CmdLine ()
import qualified Effect.Log as Log
import qualified System.Environment
import           Test.Hspec

spec :: Spec
spec = context "Config" $
    context "with valid cmdline arguments" $ before (readConfig validArgs) $ do
        context "with file parsed to Config" $ beforeWith (right . Config.parseIniFile) $ do
            it "parse config file" $ \(config, _) -> config `shouldBe` validConfig
            it "redact printed ini file" $ \(_, configStr) ->
                (List.find ("auth" `T.isPrefixOf`) . T.lines) configStr
                    `shouldBe` Just "auth: <REDACTED>"
        context "with file parsed to Ini" $ beforeWith (right . Ini.parseIni) $ do
            it "reject invalid level" $ \ini ->
                (Config.Internal.parseFromIni . update "Log" "level" (Just "invalid")) ini
                    `shouldBe` Left "Invalid log level 'invalid', choose one of Debug, \
                                    \Info, Warning, Error"
            it "reject invalid port" $ \ini ->
                (Config.Internal.parseFromIni . update "Redis" "port" (Just "invalid")) ini
                    `shouldBe` Left "input does not start with a digit"
            it "reject invalid connectTimeout" $ \ini ->
                (Config.Internal.parseFromIni . update "Redis" "connectTimeout" (Just "invalid")) ini
                    `shouldBe` Left "input does not start with a digit"
            it "reject ini with missing mandatory keys" $ \ini -> do
                let mandatoryKeys = [("Log", "level"), ("Redis", "host"), ("Redis", "port")]
                    parseWithRemovedKey (section, key) =
                        Config.Internal.parseFromIni . update section key Nothing $ ini
                mapM_ (\sectionAndKey -> parseWithRemovedKey sectionAndKey `shouldBe`
                         (Left . T.unpack)
                         ("Couldn't find key: " <> fst sectionAndKey <> "." <> snd sectionAndKey))
                      mandatoryKeys
  where
    -- update key in section with value (or delete key)
    update section key mValue = Ini.Ini . HashMap.adjust (HashMap.update (const mValue) key) section
                                        . Ini.unIni
    readConfig = flip System.Environment.withArgs
               $ CmdLine.parseCommandLineOptions >>= T.readFile . CmdLine.cmdLineConfigFile
    right e = (e `shouldSatisfy` isRight) >> either (error "left") return e
    validConfig = Config.Config (Config.Log (Just "out.log") Log.Info)
                                (Config.Redis "localhost" 8888 (Just "secret pwd") (Just 10.5))
    validArgs = ["-c", fixturesDir <> "valid.ini"]
    fixturesDir = "test/fixtures/"
