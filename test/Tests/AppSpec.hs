module Tests.AppSpec (spec) where

import qualified App
import qualified Config
import qualified Control.Concurrent as C
import qualified Control.Concurrent.Async as CA
import           Control.Monad (void)
import           Control.Monad.Reader (runReaderT)
import qualified Data.ByteString.Char8 as BS
import qualified GracefulShutdown
import qualified Log
import qualified Mock.Environment
import qualified System.Environment
import qualified System.Posix.Signals as PS
import qualified System.Log.FastLogger as FL
import           Test.Hspec

spec :: Spec
spec = context "App" $
    context "with valid config" $ before (createEnvironment "valid.cfg") $
        context "with application started" $ beforeWith startApplication $ do
            context "with INT signal sent" $ beforeWith raiseSigInt $
                context "with application done" $ beforeWith waitForApplicationDone $
                    it "shutted down with goodbye message" $ \logEntries -> do
                        logEntries `shouldSatisfy` 
                            ((Log.Info, "Caught signal 2, shutting down...") `elem`)
                        head logEntries `shouldBe` (Log.Info, "Shutdown complete")
            context "with shutdown requested" $ beforeWith requestShutdown $
                context "with application done" $ beforeWith waitForApplicationDone $
                    it "started with hello message" $ \logEntries -> do
                        let firstLogEntry = last logEntries
                        (toString . snd) firstLogEntry `shouldStartWith` "Startup version "
                        fst firstLogEntry `shouldBe` Log.Info
  where
    waitForApplicationDone (_, logSink, appAsync) = do
        -- wait for application to finish
        void $ CA.wait appAsync

        -- return log entries
        C.readMVar logSink
    requestShutdown arg@(env, _, _) = do
        runReaderT GracefulShutdown.request env
        return arg
    raiseSigInt arg = PS.raiseSignal PS.sigINT >> return arg
    startApplication (env, logSink) = do
        runReaderT App.installSignalHandlers env

        -- start application asynchronously
        appAsync <- CA.async $ runReaderT App.run env

        return (env, logSink, appAsync)
    createEnvironment configFile = do
        logSink <- C.newMVar []
        env <- System.Environment.withArgs ["-c", fixturesDir <> configFile]
              $ Config.parseCommandLineOptions
            >>= Config.parseConfigFile . Config.cmdLineConfigFile
            >>= Mock.Environment.create logSink
        return (env, logSink)
    fixturesDir = "test/fixtures/"
    toString = BS.unpack . FL.fromLogStr
