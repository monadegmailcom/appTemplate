module Tests.AppSpec (spec) where

import qualified App
import qualified Config
import qualified Control.Concurrent as C
import qualified Control.Concurrent.Async as CA
import qualified Control.Exception as E
import           Data.Maybe (isJust)
import qualified Data.Version as Version
import qualified Log
import qualified Mock.Environment
import           MultiReader ((<:>))
import qualified MultiReader as MR
import qualified Paths_appTemplate as Paths
import qualified State
import qualified System.Environment
import qualified System.Posix.Signals as PS
import qualified System.Log.FastLogger as FL
import           Test.Hspec

spec :: Spec
spec = context "App" $
    context "with valid config" $ before (createEnvironment "valid.cfg") $
        context "with application started" $ beforeWith startApplication $
            context "with INT signal sent" $ beforeWith raiseSigInt $
                context "with application done" $ beforeWith waitForApplicationDone $
                    it "logs as expected" $ \logEntries -> do
                        head logEntries `shouldBe` (Log.Info, "Startup version " <> version)
                        last logEntries `shouldBe` (Log.Info, "Shutdown complete")
                        -- note: we expect "U ..done" to follow terminatation signal because
                        -- this event is uninterruptable
                        dropWhile (/= (Log.Info, "Caught terminate signal 2")) logEntries
                            `shouldSatisfy` ((Log.Info, "U ..done") `elem`)
  where
    version = FL.toLogStr . Version.showVersion $ Paths.version
    waitForApplicationDone (_, logSink, appAsync) = do
        -- wait for application to finish
        CA.wait appAsync `shouldThrow` isJust @E.SomeAsyncException . E.fromException
        -- return log entries and chronologic order
        reverse <$> C.readMVar logSink
    raiseSigInt arg = PS.raiseSignal PS.sigINT >> return arg
    startApplication (env, logSink) = do
        -- start application asynchronously
        appAsync <- CA.async $ MR.runReader env App.run
        MR.runReader env $ App.installSignalHandlers $ CA.asyncThreadId appAsync

        return (env, logSink, appAsync)
    createEnvironment configFile = do
        logSink <- C.newMVar []
        let logFunction = Mock.Environment.createLogFunction logSink
        config <- System.Environment.withArgs ["-c", fixturesDir <> configFile] $
                  Config.parseCommandLineOptions
              >>= Config.parseConfigFile . Config.cmdLineConfigFile
        state <- State.defaultState
        return (config <:> logFunction <:> MR.makeList state, logSink)
    fixturesDir = "test/fixtures/"

