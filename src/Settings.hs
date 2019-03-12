{-| The application's settings contain the global variables and mockable external environment.
    Read-write variable should be wrapped in some kind of globally accessible type as `IORef` or
    `MVar` (for synchronized access).
-}

module Settings
    ( Settings(..)
    , create
    ) where

import qualified Control.Concurrent as C

import Environment (Environment(..))

import qualified Log

{-| Application's global state.
    Includes a "busy" variable, if set, the application should not be interrupted, if empty
    it indicates an interruptable "idle" state. It is used by the signal handlers to process
    a gracefull shutdown. 
    Likewise a set "shutdown" variable may prevent the application to start new tasks. 
-}
data Settings = Settings
    { settingsEnvironment :: Environment -- ^ external environment
    , settingsBusy :: !(C.MVar ()) -- ^ indicate application's busy state
    , settingsShutdown :: !(C.MVar ()) -- ^ indicate application's shutdown state
    }

instance Log.HasLog Settings where
    getLogFunction = Environment.envLogFunction . settingsEnvironment

-- | Create production settings
create :: Environment -> IO Settings
create env = do
    -- set to "busy" initially, App.run is expected to set this to "idle" after initializing.
    busy <- C.newMVar ()

    Settings env busy <$> C.newEmptyMVar
