-- disable warning concerning Redis.PortID, I cannot do anything about it
{-# OPTIONS_GHC -Wno-deprecations #-}

module Tests.RedisSpec where

import qualified Config
import qualified Effect.Database as Database
import qualified Effect.Database.Impl.Redis as Redis
import qualified Effect.Database.Init as Database
import qualified Effect.Log as Log
import qualified Effect.Log.Impl.List as List
import qualified Effect.Log.Init as Log
import qualified Helper.Redis

import qualified Control.Concurrent as C
import qualified Control.Exception.Safe as E
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (asks, runReaderT, ReaderT)
import qualified Data.List as List
import qualified Data.Text.Lazy as TL
import qualified Database.Redis as R
import qualified System.Process as Process
import           Test.Hspec
import qualified Test.Hspec.Expectations.Lifted as L
import qualified Time.Units

-- test context
data Env = Env { envConnectInfo :: R.ConnectInfo
               , envRedis :: C.MVar Redis.Resource
               , envLog :: C.MVar List.Resource
               }

type App = ReaderT Env IO

instance Redis.HasResource App where getResource = asks envRedis
instance List.HasResource App where getResource = asks envLog

spec :: Spec
spec = context "Redis" $ beforeAll setup $
    it "retries redis operation on failure" $ \env -> do
        -- start redis server, initialize redis effect and shutdown server
        E.bracket (Helper.Redis.startServer . envConnectInfo $ env) 
                  Process.terminateProcess $ const $ flip runReaderT env $ do
            asks envConnectInfo >>= Database.init . Config.Redis
            Log.init Log.Info Log.StdOut

        -- we seem to have to wait some time for the redis service to actually shutdown
        Time.Units.threadDelay $ Time.Units.sec 0.1

        -- now the redis server is gone, we expect the redis get operation to be retried once and
        -- then giving up, rethrowing the original exception
        flip runReaderT env $ do
            E.try (Database.getByKey "any") >>= (`L.shouldSatisfy`
                either (maybe False
                               (\e -> "Network.Socket.connect:" `List.isPrefixOf` show e)
                         . E.fromException @E.IOException)
                       (const False))
            getLog `L.shouldReturn` [ (Log.Warning,"Redis action failed: ConnectionLost")
                                    , (Log.Info,"Reconnect and try again")]

        -- we restart the redis server and retry the get operation
        E.bracket (Helper.Redis.startServer . envConnectInfo $ env) 
                  Process.terminateProcess $ const $ flip runReaderT env $
            Database.getByKey "any" `L.shouldReturn` Nothing

getLog :: App [(Log.Level, TL.Text)]
getLog = asks envLog >>= fmap (reverse . List.resourceSink) . liftIO . C.readMVar

setup :: IO Env
setup = do
    let connectInfo = R.defaultConnectInfo
    port <- Helper.Redis.getFreePort . R.connectHost $ connectInfo
    let patchedConnectInfo = connectInfo { R.connectPort = R.PortNumber port }
    Env patchedConnectInfo <$> C.newEmptyMVar <*> C.newEmptyMVar
