-- disable warning concerning Redis.PortID, I cannot do anything about it
{-# OPTIONS_GHC -Wno-deprecations #-}

module Tests.RedisSpec where

import qualified Effect.Redis as Redis
import           Effect.Redis.Impl ()
import qualified Effect.Filesystem as FS
import qualified Effect.Filesystem.Memory as MFS
import           Effect.Concurrent.Thread.Impl ()
import           Effect.Concurrent.STM.Impl ()
import           Effect.Time.Impl ()
import qualified Helper.Redis
import qualified Log

import qualified Control.Concurrent as C
import qualified Control.Concurrent.STM as C
import qualified Control.Exception.Safe as E
import           Control.Monad ((>=>), void)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (asks, runReaderT, ReaderT)
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy as TL
import qualified Database.Redis as R
import qualified System.Process as Process
import           Test.Hspec
import qualified Test.Hspec.Expectations.Lifted as L
import qualified Time.Units

-- test context
data Env = Env { envConnectInfo :: R.ConnectInfo
               , envFs :: C.MVar (Map.Map FilePath [BL.ByteString])
               , envLogChan :: C.TMVar (Maybe Log.Msg)
               }

type App = ReaderT Env IO

instance MFS.HasResource App where getResource = asks envFs
instance R.MonadRedis App where liftRedis = R.liftRedis
instance R.RedisCtx App (Either R.Reply) where returnDecode = R.returnDecode

retryOnce :: E.MonadCatch m => (E.SomeException -> m ()) -> m a -> m a
retryOnce handleException action = E.catchAny action (handleException >=> const action)

serialLog :: Log.Level -> TL.Text -> App ()
serialLog level str = do
    handle <- FS.openFile "test" FS.AppendMode
    msg <- Log.toMsg level str
    Log.logger handle msg

spec :: Spec
spec = context "Redis" $ beforeAll setup $
    it "retries redis operation on failure" $ \env -> do
        -- start redis server, initialize redis effect and shutdown server
        connection <- E.bracket (Helper.Redis.startServer . envConnectInfo $ env)
                  Process.terminateProcess $ const $ flip runReaderT env $ do
            asks envConnectInfo >>= Redis.connect

        -- we seem to have to wait some time for the redis service to actually shutdown
        Time.Units.threadDelay $ Time.Units.sec 0.1

        -- now the redis server is gone, we expect the redis get operation to be retried once and
        -- then giving up, rethrowing the original exception
        let reconnect e = do
                serialLog Log.Warning $ "Redis action failed: " <> (TL.pack . show) e
                           <> "\nReconnect and try again"
                asks envConnectInfo >>= void . Redis.connect

        flip runReaderT env $ do
            E.try (retryOnce reconnect $ Redis.get connection "any") >>= (`L.shouldSatisfy`
                either (maybe False
                               (\e -> "Network.Socket.connect:" `List.isPrefixOf` show e)
                         . E.fromException @E.IOException)
                       (const False))
            allLines <- getLog
            length allLines `L.shouldBe` 1
            let line = TL.unpack . head $ allLines
            line `L.shouldContain` "Redis action failed: ConnectionLost\nReconnect and try again"
            line `L.shouldContain` "Warning"

        -- we restart the redis server and retry the get operation
        E.bracket (Helper.Redis.startServer . envConnectInfo $ env)
                  Process.terminateProcess $ const $ flip runReaderT env $
            Redis.get connection "any" `L.shouldReturn` Nothing

getLog :: App [TL.Text]
getLog = map TL.decodeUtf8 . maybe [] reverse . Map.lookup "test"
     <$> (asks envFs >>= liftIO . C.readMVar)

setup :: IO Env
setup = do
    let connectInfo = R.defaultConnectInfo
    port <- Helper.Redis.getFreePort . R.connectHost $ connectInfo
    let patchedConnectInfo = connectInfo { R.connectPort = R.PortNumber port }
    Env patchedConnectInfo <$> C.newMVar Map.empty <*> C.newEmptyTMVarIO
