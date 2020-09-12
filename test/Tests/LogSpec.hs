module Tests.LogSpec (spec) where

import qualified Effect.Log as Log
import qualified Effect.Log.Impl.FastLogger as FastLogger

import           Control.Monad.Reader (ReaderT, ask, runReaderT)
import           Control.Monad.Trans.Control (control)
import qualified System.IO.Silently as Silently
import           Test.Hspec
import qualified Test.Hspec.Expectations.Lifted as L

type MyLogM = ReaderT FastLogger.Resource IO
instance FastLogger.HasResource MyLogM where getResource = ask

testInfo :: MyLogM ()
testInfo
     = control (\runInIO -> Silently.capture_ (   runInIO (Log.info "hi" 
                                               >> FastLogger.flush)))
   >>= (`L.shouldContain` "hi")

testWarning :: MyLogM ()
testWarning
    = control (\runInIO -> Silently.capture_ (   runInIO (Log.warning "hi" 
                                              >> FastLogger.flush)))
  >>= (`L.shouldContain` "hi")

testDebug :: MyLogM ()
testDebug
    = control (\runInIO -> Silently.capture_ (   runInIO (Log.debug "hi" 
                                              >> FastLogger.flush)))
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
