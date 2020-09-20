{- | Contains the application's implementation. -}
module App.Impl
    () where

import           Effect.CmdLine.Impl ()
import           Effect.Redis.Impl ()
import           Effect.Filesystem.Impl ()
import           Effect.Signal.Impl ()
import           Effect.Concurrent.Thread.Impl ()
import           Effect.Concurrent.Stream.Impl ()
import           Effect.Concurrent.STM.Impl ()
import           Effect.Time.Impl ()
