-- disable warning concerning Redis.PortID, I cannot do anything about it
{-# OPTIONS_GHC -Wno-deprecations #-}

{- | Helper functions for redis tests. -}
module Helper.Redis
    ( getFreePort
    , startServer
    ) where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS
import           Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Database.Redis as R
import qualified GHC.IO.Handle as Handle
import qualified Network.Socket as Socket
import qualified System.Process as Process
import qualified Time.Units

-- | Ask os for free socket.
getFreePort :: Socket.HostName -> IO Socket.PortNumber
getFreePort host = E.bracket (Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol)
                             Socket.close $ \sock -> do
    address <- Socket.getAddrInfo Nothing (Just host) Nothing
           >>= maybe (error "getAddrInfo failed") (return . Socket.addrAddress) . listToMaybe
    Socket.bind sock address
    Socket.getSocketName sock >>= \case
        Socket.SockAddrInet port _ -> return port
        _ -> error "Unable to get free socket port"
--  where
--    address = Socket.SockAddrInet 0 $ Socket.tupleToHostAddress (127, 0, 0, 1)

-- | Start redis server on localhost.
startServer :: R.ConnectInfo -> IO Process.ProcessHandle
startServer connectInfo = do
    -- start redis process on free port and redirect stdout to a pipe
    (mInHandle, mOutHandle, _, processHandle) <- Process.createProcess $
         process { Process.std_out = Process.CreatePipe, Process.std_in = Process.CreatePipe }
    do -- write redis config to stdin
        let inHandle = fromMaybe (error "no in handle") mInHandle
            configStr = case R.connectPort connectInfo of
                            R.PortNumber port -> "port " ++ show port ++ "\n"
                            _ -> error "no port defined"
                        ++ maybe "" (("requirepass " ++) . BS.unpack) (R.connectAuth connectInfo)
        Handle.hPutStr inHandle configStr
        Handle.hClose inHandle -- the redis server starts when stdin closes
    do -- wait (with timeout) for redis server to be ready accepting connections
        let outHandle = fromMaybe (error "no out handle") mOutHandle
        Time.Units.timeout startupTimeout (outHandle `waitFor` "Ready to accept connections")
            >>= maybe (error "mongod not ready within timeout") return
    return processHandle
  where
    process = Process.proc "redis-server" ["-"]
    startupTimeout = Time.Units.sec 5

-- wait until trigger string is read from handle
waitFor :: Handle.Handle -> T.Text -> IO ()
waitFor handle str = snd . T.breakOn str <$> T.hGetLine handle >>= \case
    "" -> waitFor handle str -- substring not found, keep waiting
    _ -> return () -- substring found, exit loop
