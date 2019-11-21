{- | Contains the application's implementation. -}
module App.Impl
    ( defaultResource
    ) where

import qualified Control.Concurrent as C
import           Control.Monad.Reader (ReaderT, asks)
import           Effect.CmdLine.Impl ()
import qualified Effect.Database.Impl.Redis as Redis
import           Effect.Filesystem.Impl ()
import qualified Effect.Log.Impl.FastLogger as FastLogger
import           Effect.Signal.Impl ()
import qualified Effect.State.Impl as State
import           Effect.Thread.Impl ()

{- | The application resources for effect implementations. -}
data Resource = Resource
    { resourceLog :: !(C.MVar FastLogger.Resource)
    , resourceState :: !State.Resource
    , resourceRedis :: !(C.MVar Redis.Resource)
    }

{- | Build default resource. Note: some resources must be initialized before usage. E.g. before
     using "Effect.Database" use "Effect.Database.Init.InitM" for database initialization. -}
defaultResource :: IO Resource
defaultResource = Resource <$> C.newEmptyMVar <*> State.defaultResource <*> C.newEmptyMVar

type App = ReaderT Resource IO

-- give application access to logging, state and redis effects.
-- access to cmdline, filesystem, signal and thread effects are given by module imports above
instance FastLogger.HasResource App where getResource = asks resourceLog
instance State.HasResource App where getResource = asks resourceState
instance Redis.HasResource App where getResource = asks resourceRedis



