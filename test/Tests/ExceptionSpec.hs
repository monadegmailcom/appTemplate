{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tests.ExceptionSpec (spec) where

import qualified Effect.Redis as Redis

import qualified Control.Exception.Safe as E
import qualified Data.ByteString as BS
import           Test.Hspec

-- trivial synchronous exception throwing implementation
instance Redis.RedisM IO () where
    connect = undefined
    runRedis _ _ = E.throwString "not yet implemented"

-- catch exception in pure context
getValue :: (E.MonadCatch m, Redis.RedisM m ()) => m (Either String BS.ByteString)
getValue = E.catch
    (maybe (Left "Not found") Right <$> Redis.get () "hello")
    $ \(E.StringException msg _) -> return $ Left msg

spec :: Spec
spec = context "Exception" $
    it "catches effect exception in pure context" $ getValue `shouldReturn` Left "not yet implemented"
