{-# OPTIONS_GHC -fno-warn-orphans #-}

{- | Contains the application's implementation. Address following requirements:

     - 'installSignalHandlers' to allow a graceful shutdown
     - 'runPollers' start the application's implementation threads asynchronously
-}
module App.Impl
    ( App
    , Env(..)
    , app
    , initializeEnv
    , installSignalHandlers
    , runPollers
    ) where

import qualified Poll
import qualified Config
import qualified Control.Concurrent as C
import qualified Control.Concurrent.Async as CA
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as E
import qualified Control.Exception.Safe as E.Safe
import           Control.Monad (forever, void)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ReaderT, asks)
import           Control.Monad.State (MonadState, StateT, get, gets, put, modify)
import           Control.Monad.Trans.Control (control)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Version as Version
import qualified Database.Redis as Redis
import qualified Effect.CmdLine as CmdLine
import qualified Effect.CmdLine.Impl ()
import qualified Effect.Database.Impl as Database.Impl
import qualified Effect.Database.Init as Database.Init
import qualified Effect.Log as Log
import qualified Effect.Log.Impl as Log.Impl
import qualified Effect.State.Impl as State.Impl
import qualified Effect.Thread.Impl as Thread.Impl ()
import           Formatting ((%))
import qualified Formatting as F
import qualified Paths_appTemplate as Paths
import qualified System.Posix.Signals as PS

{- | The application's effect implementation. -}
data Env = Env
    { envLogFunction :: !(STM.TVar Log.Impl.Function)
    , envState :: !State.Impl.State
    , envRedis :: !(Maybe Database.Impl.Redis)
    }

-- | Encapsulate the application's access to effects.
type App = StateT Env IO

-- give application access to logging
instance Log.Impl.HasLog App where
    getLogFunction = gets envLogFunction >>= liftIO . STM.readTVarIO
    setLogFunction logFunction = do
        tvar <- gets envLogFunction
        liftIO . STM.atomically $ STM.writeTVar tvar logFunction

-- give application access to state
instance State.Impl.HasState App where
    getState = gets envState

-- give application access to redis
instance Database.Impl.RedisM App where
    get = gets envRedis >>= maybe (error "Redis not initialized") return
    set redis = modify $ \env -> env { envRedis = Just redis }

-- Customize ini file related exceptions
newtype IniFileException = IniFileException String deriving (Show, E.Exception)

initializeEnv :: IO Env
initializeEnv = do
           -- parse command line for config file
    content <- CmdLine.parseCommandLineOptions
           -- read config file accordingly
       >>= T.readFile . CmdLine.cmdLineConfigFile
           -- parse config file syntactically to ini type
    -- define environment with effect implementations
    state <- State.Impl.defaultState
    nullLog <- STM.newTVarIO (\_ _ -> return ())
    return $ App.Impl.Env nullLog state Nothing

-- Application entry point.
app :: T.Text -> App ()
app content = do
    -- parse configuration semantically from ini type
    (config, prettyContent) <-
        either (E.Safe.throwM . IniFileException) return . Config.parseIniFile $ content
    -- build logger from configuration
    let Config.Log logDestination logLevel = Config.configLog config
    Log.init logLevel logDestination
    -- log current (read-only) configuration from pretty printed ini type
    Log.info . F.format ("Initial configuration\n" % F.stext) $ prettyContent
    -- termination signals should be thrown as async exception to this thread
    liftIO C.myThreadId >>= App.Impl.installSignalHandlers
    -- log greeting message with version info
    Log.info . F.format ("Startup version " % F.string) $ Version.showVersion Paths.version
    -- test redis connection on startup
    Log.info "Ping redis connection.."
    Database.Init.init $ Config.configRedis config
    Database.Impl.runRedis Redis.ping >>= Log.info . F.format ("Redis reply: " % F.shown)
    -- the application is up and running now
    Log.info "Application initialized"
    -- call pollers forever and log goodbye message finally
    -- if this thread receives as async exception all pollers are cancelled
    -- if either poller throws an exception all sibling pollers are cancelled
    control $ \runInIO -> E.finally (runInIO App.Impl.runPollers)
                                    (runInIO $ Log.info "Shutdown complete")

{- | Start poller asynchronously. Poller will terminate on asynchronous exceptions.
     If either poller throws an exception all sibling pollers will terminate. -}
runPollers :: App ()
runPollers =
    -- note: one of the poller is run masked uninterruptable, it will
    -- delay termination until next iteration by 'forever'
    control $ \runInIO -> let pollState = runInIO . Poll.pollState
                              pollRedis = runInIO . Poll.pollRedis
                          in CA.mapConcurrently_ forever
                              [ E.uninterruptibleMask_ $ pollState "U"
                              , pollState "S"
                              , pollRedis "R1"
                              , pollRedis "R2"
                              ]

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
