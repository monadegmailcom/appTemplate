{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tests.ExceptionSpec (spec) where

import qualified Effect.Database as Database

import qualified Control.Exception.Safe as E
import qualified Data.ByteString as BS
import           Test.Hspec

-- trivial synchronous exception throwing implementation
instance Database.DatabaseM IO where
    getByKey _ = E.throwString "not yet implemented"
    setByKey _ _ = return ()

-- catch exception in pure context
getValue :: (E.MonadCatch m, Database.DatabaseM m) => m (Either String BS.ByteString)
getValue = E.catch
    (maybe (Left "Not found") Right <$> Database.getByKey "hello")
    $ \(E.StringException msg _) -> return $ Left msg

spec :: Spec
spec = context "Exception" $
    it "catches effect exception in pure context" $ getValue `shouldReturn` Left "not yet implemented"
