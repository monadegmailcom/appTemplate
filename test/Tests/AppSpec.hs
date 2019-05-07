module Tests.AppSpec (spec) where

import qualified App
import qualified Config
import qualified Control.Concurrent as C
import qualified Control.Concurrent.Async as CA
import qualified Control.Exception as E
import           Data.Maybe (isJust)
import qualified Data.Text.Lazy as TL
import qualified Data.Version as Version
import qualified Log
import qualified Mock.Environment
import qualified Paths_appTemplate as Paths
import qualified State
import qualified System.Environment
import qualified System.Posix.Signals as PS
import           Test.Hspec

spec :: Spec
spec = context "App" $
    context "with valid config" $ before (createEnvironment "valid.cfg") $
        context "with application started" $ beforeWith startApplication $
            context "with USR1 and INT signals sent" $ beforeWith
                (\env -> mapM_ PS.raiseSignal [PS.sigUSR1, PS.sigINT] >> return env) $
                context "with application done" $ beforeWith waitForApplicationDone $
                    it "logs as expected" $ \logEntries -> do
                        head logEntries `shouldBe` (Log.Info, "Startup version " <> version)
                        last logEntries `shouldBe` (Log.Info, "Shutdown complete")
                        -- see http://man7.org/linux/man-pages/man7/signal.7.html
                        logEntries `shouldSatisfy` ((Log.Info, "Caught signal 10, ignore") `elem`)
                        -- note: we expect "U ..done" to follow termination signal because
                        -- this event is uninterruptable
                        dropWhile (/= (Log.Info, "Caught signal 2, terminate")) logEntries
                            `shouldSatisfy` ((Log.Info, "U ..done") `elem`)
  where
    version = TL.pack . Version.showVersion $ Paths.version
    waitForApplicationDone (_, _, _, logSink, appAsync) = do
        -- wait for application to finish
        CA.wait appAsync `shouldThrow` isJust @E.SomeAsyncException . E.fromException
        -- return log entries and chronologic order
        reverse <$> C.readMVar logSink
    startApplication (logFunction, config, state, logSink) = do
        -- start application asynchronously
        appAsync <- CA.async $ App.run logFunction config state
        App.installSignalHandlers logFunction $ CA.asyncThreadId appAsync

        return (logFunction, config, state, logSink, appAsync)
    createEnvironment configFile = do
        logSink <- C.newMVar []
        let logFunction = Mock.Environment.createLogFunction logSink
        config <- System.Environment.withArgs ["-c", fixturesDir <> configFile] $
                  Config.parseCommandLineOptions
              >>= Config.parseConfigFile . Config.cmdLineConfigFile
        state <- State.defaultState
        return (logFunction, config, state, logSink)
    fixturesDir = "test/fixtures/"

