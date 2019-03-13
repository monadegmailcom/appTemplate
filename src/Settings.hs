{-| The application's settings contain its global state.
    Read-write variable should be wrapped in some kind of globally accessible type as `IORef` or
    `MVar` (for synchronized access).
-}

module Settings
    ( Settings(..)
    , create
    ) where

import qualified Control.Concurrent as C

{-| Application's global state.
    Includes a "busy" variable, if set, the application should not be interrupted, if empty
    it indicates an interruptable "idle" state. Likewise a set "shutdown" variable may prevent
    the application to start new tasks.
    It is used by the signal handlers to process a gracefull shutdown.
-}
data Settings = Settings
    { settingsBusy :: !(C.MVar ()) -- ^ indicate application's busy state
    , settingsShutdown :: !(C.MVar ()) -- ^ indicate application's shutdown state
    }

-- | Create production settings.
create :: IO Settings
create = do
    -- set to "busy" initially, App.run is expected to set this to "idle" after initializing.
    busy <- C.newMVar ()

    Settings busy <$> C.newEmptyMVar
