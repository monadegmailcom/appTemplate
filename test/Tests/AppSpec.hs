{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tests.AppSpec (spec) where

import qualified App.Impl
import qualified Config
import qualified Control.Concurrent as C
import qualified Control.Concurrent.Async as CA
import qualified Control.Exception as E
import           Control.Monad.Reader (ReaderT, ask, runReaderT)
import           Control.Monad.Trans.Control (control)
import qualified Data.Ini as Ini
import           Data.Maybe (isJust)
import qualified Data.Text.IO as T
import qualified Effect.CmdLine as CmdLine
import qualified Effect.CmdLine.Impl as CmdLine ()
import qualified Effect.Log as Log
import qualified Effect.Log.Impl as Log
import qualified Effect.State.Impl as State
import qualified Mock.Environment
import qualified System.Environment
import qualified System.Posix.Signals as PS
import           Test.Hspec

instance Log.HasLog (ReaderT Log.Function IO) where
    getLogFunction = ask

spec :: Spec
spec = context "App" $
    context "with valid config" $ before (createContext "valid.ini") $
        context "with application started" $ beforeWith startApplication $
            context "with USR1 and INT signals sent" $ beforeWith
                (\ctx -> mapM_ PS.raiseSignal [PS.sigUSR1, PS.sigINT] >> return ctx) $
                context "with application done" $ beforeWith waitForApplicationDone $
                    it "logs as expected" $ \logEntries -> do
                        -- see http://man7.org/linux/man-pages/man7/signal.7.html
                        logEntries `shouldSatisfy` ((Log.Info, "Caught signal 10, ignore") `elem`)
                        -- note: we expect "U ..done" to follow termination signal because
                        -- this event is uninterruptable
                        dropWhile (/= (Log.Info, "Caught signal 2, terminate")) logEntries
                            `shouldSatisfy` ((Log.Info, "U ..done") `elem`)
  where
    waitForApplicationDone (logSink, appAsync) = do
        -- wait for application to finish
        CA.wait appAsync `shouldThrow` isJust @E.SomeAsyncException . E.fromException
        -- return log entries and chronologic order
        reverse <$> C.readMVar logSink
    startApplication (env, _, logSink) = do
        -- start application asynchronously
        appAsync <- flip runReaderT env $ do
            async <- control $ \runInIO -> CA.async $ runInIO App.Impl.runPollers
            App.Impl.installSignalHandlers . CA.asyncThreadId $ async
            return async
        return (logSink, appAsync)
    createContext configFile = do
        logSink <- C.newMVar []
        let logFunction = Mock.Environment.createLogFunction logSink
        configContent <- System.Environment.withArgs ["-c", fixturesDir <> configFile] $
                         CmdLine.parseCommandLineOptions
                     >>= T.readFile . CmdLine.cmdLineConfigFile
        let config = either error id $ Ini.parseIni configContent >>= Config.parseIniFile
        env <- App.Impl.Env logFunction <$> State.defaultState
        return (env, config, logSink)
    fixturesDir = "test/fixtures/"

