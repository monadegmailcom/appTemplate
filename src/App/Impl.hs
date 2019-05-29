{-# OPTIONS_GHC -fno-warn-orphans #-}

{- | Contains the application's implementation. Address following requirements:

     - 'installSignalHandlers' to allow a graceful shutdown
     - 'runPollers' start the application's implementation threads asynchronously
-}
module App.Impl
    ( App
    , Env(..)
    , installSignalHandlers
    , runPollers
    ) where

import qualified Poll
import qualified Control.Concurrent as C
import qualified Control.Concurrent.Async as CA
import qualified Control.Exception as E
import           Control.Monad (forever, void)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ReaderT, asks)
import           Control.Monad.Trans.Control (control)
import qualified Effect.Log as Log
import qualified Effect.Log.Impl as Log.Impl
import qualified Effect.State.Impl as State.Impl
import qualified Effect.Thread as Thread
import qualified Effect.Thread.Impl as Thread.Impl
import           Formatting ((%))
import qualified Formatting as F
import qualified System.Posix.Signals as PS


{- | The application's effect implementation. -}
data Env = Env
    { envLogFunction :: Log.Impl.Function
    , envState :: State.Impl.State
    }

-- | Encapsulate the application's access to effects.
type App = ReaderT Env IO

-- give application access to logging
instance Log.Impl.HasLog App where
    getLogFunction = asks envLogFunction

-- give application access to state
instance State.Impl.HasState App where
    getState = asks envState

-- give application access to thread effects
instance Thread.ThreadM App where
    delay = Thread.Impl.delay

{- | Start poller asynchronously. Poller will terminate on asynchronous exceptions.
     If either poller throws an exception all sibling pollers will terminate. -}
runPollers :: App ()
runPollers = 
    -- note: one of the poller is run masked uninterruptable, it will
    -- delay termination until next iteration by 'forever'
    control $ \runInIO -> CA.mapConcurrently_ forever
        [ E.uninterruptibleMask_ $ runInIO $ Poll.poll "U"
        , runInIO $ Poll.poll "A"
        , runInIO $ Poll.poll "B"]

{- | Install signal handlers. Terminate signals are transformed to async exception thrown to
     given thread. -}
installSignalHandlers :: C.ThreadId -> App ()
installSignalHandlers threadId = 
       installHandler usr1SignalHandler PS.sigUSR1
    >> mapM_ (installHandler termSignalHandler) terminateSignals
  where
    toStr signalInfo msg = 
        let signal = PS.siginfoSignal signalInfo
        in F.format ("Caught signal " % F.shown % ", " % F.text) signal msg
    usr1SignalHandler signalInfo = Log.info $ toStr signalInfo "ignore"
    termSignalHandler signalInfo = do
        Log.info $ toStr signalInfo "terminate"
        liftIO $ E.throwTo threadId E.UserInterrupt
    -- list of signals on which we want to terminate, this conforms to the linux defaults,
    -- see http://man7.org/linux/man-pages/man7/signal.7.html
    -- exception is SIGPIPE, which we want to ignore
    terminateSignals = [PS.sigHUP, PS.sigINT, PS.sigALRM, PS.sigTERM]

-- install handler with signal informations as arguments
installHandler :: (PS.SignalInfo -> App ()) -> PS.Signal -> App ()
installHandler handler signal = control $ \runInIO -> void $ PS.installHandler
    signal
    (PS.CatchInfo $ \signalInfo -> runInIO (handler signalInfo))
    Nothing
