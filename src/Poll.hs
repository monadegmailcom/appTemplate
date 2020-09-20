{- | Application's polling events. -}
module Poll
    ( runState
    , runRedis
    ) where

import qualified Effect.Redis as Redis
import qualified Effect.Concurrent.STM as STM
import qualified Log

import qualified Control.Exception.Safe as E
import           Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BS
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.Read as TL
import qualified Data.Text.Encoding.Error as EE
import           Formatting ((%))
import qualified Formatting as F

-- | Process state event.
runState :: Log.LogM m => Log.Resource m -> STM.TVar m Int -> m ()
runState logRes stateR = do
    state <- STM.atomically $ do
        i <- STM.readTVar stateR
        STM.writeTVar stateR $ i + 1
        return i
    Log.info logRes . F.format ("Processed state " % F.int) $ state

data ContentError =
      KeyNotPresent
    | ValueToBig BS.ByteString
    | ValueNotUtf8 BS.ByteString EE.UnicodeException
    | ValueNotNumeric BS.ByteString String
    deriving Show

-- | Process redis event.
runRedis :: ( E.MonadCatch m, Log.LogM m, Redis.RedisM m c)
         => Log.Resource m -> c -> m ()
runRedis logRes connection = do
    Log.debug logRes $ F.format ("Read redis key " % F.shown) key

    -- parse value from redis and handle invalid content
    value <- Redis.get connection key
         >>= either onContentError return . parseValue

    -- increment value and build string
    let new = BL.toStrict . BS.toLazyByteString . BS.intDec $ value + 1

    Log.debug logRes
       $ F.format ("Write redis key " % F.shown % " with value " % F.shown)
                  key new
    Redis.set connection key new

    Log.info logRes $ F.format ("Processed redis " % F.int) value
  where
    onContentError (e :: ContentError) = do
        Log.warning logRes
            $ F.format (  "Error reading redis value from key "
                        % F.shown % ": " % F.shown % ", set to default "
                        % F.int) key e def
        return def
    parseValue mValue = -- Either ContentError monad, pure parsing
             maybe (Left KeyNotPresent)
                   (Right . BS.splitAt maxValueLength)
                   mValue
         >>= \(bs, trailing) -> (if BS.null trailing
                                 then Right $ BL.fromStrict bs
                                 else Left KeyNotPresent)
         >>= first (ValueNotUtf8 bs) . TL.decodeUtf8'
         >>= first (ValueNotNumeric bs) . TL.decimal
         >>= Right . fst
    key = "msg"
    def = 1 :: Int
    maxValueLength = 100 -- sanity check
