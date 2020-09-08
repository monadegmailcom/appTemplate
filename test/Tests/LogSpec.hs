module Tests.LogSpec (spec) where

import qualified Effect.Log as Log
import qualified Effect.Log.Impl.FastLogger as FastLogger

import           Control.Monad.Reader (ReaderT, ask, runReaderT)
import           Control.Monad.Trans.Control (control)
import qualified System.IO.Silently as Silently
import           Test.Hspec
import qualified Test.Hspec.Expectations.Lifted as L
import qualified Time.Units

type MyLogM = ReaderT FastLogger.Resource IO
instance FastLogger.HasResource MyLogM where getResource = ask

sleep :: IO ()
sleep = Time.Units.threadDelay $ Time.Units.sec 0.1

testInfo :: MyLogM ()
testInfo
     = control (\runInIO -> Silently.capture_ (runInIO (Log.info "hi") >> sleep))
   >>= (`L.shouldContain` "hi")

testWarning :: MyLogM ()
testWarning
    = control (\runInIO -> Silently.capture_ (runInIO (Log.warning "hi") >> sleep))
  >>= (`L.shouldContain` "hi")

testDebug :: MyLogM ()
testDebug
    = control (\runInIO -> Silently.capture_ (runInIO (Log.debug "hi") >> sleep))
  >>= (`L.shouldBe` "")

spec :: Spec
spec = context "Log" $
    context "with FastLogger implementation" $ beforeAll buildFastLoggerRunner $ do
        it "includes info messages" $ runReaderT testInfo
        it "includes warning messages" $ runReaderT testWarning
        it "excludes debug messages" $ runReaderT testDebug
  where
    buildFastLoggerRunner = do
        env <- FastLogger.def
        runReaderT (Log.init Log.Info Log.StdOut) env
        return env
