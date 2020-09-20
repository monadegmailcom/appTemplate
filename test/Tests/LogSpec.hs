module Tests.LogSpec (spec) where

import qualified Log
import qualified Effect.Filesystem as Filesystem
import           Effect.Filesystem.Impl ()
import qualified Effect.Concurrent.STM as STM
import           Effect.Concurrent.STM.Impl ()
import           Effect.Concurrent.Thread.Impl ()
import           Effect.Concurrent.Stream.Impl ()
import           Effect.Time.Impl ()

import qualified System.IO.Silently as Silently
import qualified Streamly.Prelude as S
import           Test.Hspec

flush :: Log.Resource IO -> IO ()
flush resource = do
    logHandle <- Filesystem.stdout
    S.drain . S.mapM (Log.logger logHandle)
            . S.takeWhile (/= Log.Flush)
            . Log.stream
            . Log.resourceMsgChan
            $ resource

testInfo :: Log.Resource IO -> IO ()
testInfo resource
     = Silently.capture_ (Log.info resource "hi" >> flush resource)
   >>= (`shouldContain` "hi")

testWarning :: Log.Resource IO -> IO ()
testWarning resource
    = Silently.capture_ (Log.warning resource "hi" >> flush resource)
  >>= (`shouldContain` "hi")

testDebug :: Log.Resource IO -> IO ()
testDebug resource
    = Silently.capture_ (Log.debug resource "hi" >> flush resource)
  >>= (`shouldBe` "")

spec :: Spec
spec = context "Log" $
    context "with FastLogger implementation" $ beforeAll setup $ do
        it "includes info messages" testInfo
        it "includes warning messages" testWarning
        it "excludes debug messages" testDebug
  where
    setup = do
       msgRelay <- STM.newTChan'
       minLevelRelay <- STM.newTVar' Log.Info
       return $ Log.Resource msgRelay minLevelRelay

