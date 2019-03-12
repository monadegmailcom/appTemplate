module Tests.AppSpec (spec) where

import App (run)

import qualified Config

import qualified Control.Concurrent as C
import qualified Control.Concurrent.Async as CA
import Control.Monad (void)
import Control.Monad.Reader (runReaderT)

import qualified Data.ByteString.Char8 as BS

import qualified Log

import qualified Mock.Environment

import qualified Settings
import Settings (Settings(..))

import qualified System.Environment
import qualified System.Posix.Signals as PS
import qualified System.Log.FastLogger as FL

import Test.Hspec

spec :: Spec
spec = context "App" $
    context "with valid settings" $ before (createSettings "valid.cfg") $
        context "with application started" $ beforeWith startApplication $ do
            context "with shutdown requested" $ beforeWith requestShutdown $
                it "starts with hello message" $ \logEntries -> do
                    let firstLogEntry = last logEntries
                    (toString . snd) firstLogEntry `shouldStartWith` "Startup version "
                    fst firstLogEntry `shouldBe` Log.Info
            context "with INT signal sent" $ beforeWith terminateApplication $
                it "shuts down with goodbye message" $ \logEntries -> do
                    let (e1, e2) = case logEntries of
                            (i1:i2:_) -> (i1, i2)
                            _ -> error "invalid log entries"
                    e1 `shouldBe` (Log.Info, "Shutdown complete")
                    e2 `shouldBe` (Log.Info, "Caught signal 2, shutting down...")
  where
    requestShutdown (settings, logSink, _) = do
        C.putMVar (settingsBusy settings) ()
        C.putMVar (settingsShutdown settings) ()
        C.readMVar logSink
    terminateApplication (_, logSink, appAsync) = do
        -- raise signal INT to let signal handler terminate application thread
        PS.raiseSignal PS.sigINT

        -- wait for application to finish
        void $ CA.wait appAsync

        C.readMVar logSink
    startApplication (settings, logSink) = do
        -- start application asynchronously
        appAsync <- CA.async $ runReaderT App.run settings

        -- wait until application initialization is finished
        C.readMVar $ settingsBusy settings

        return (settings, logSink, appAsync)
    createSettings configFile = do
        putStrLn "createSettings"
        logSink <- C.newMVar []
        settings <- System.Environment.withArgs ["-c", fixturesDir <> configFile]
              $ Config.parseCommandLineOptions
            >>= Config.parseConfigFile . Config.cmdLineConfigFile
            >>= Mock.Environment.create logSink
            >>= Settings.create
        return (settings, logSink)
    fixturesDir = "test/fixtures/"
    toString = BS.unpack . FL.fromLogStr
