{- | Command line effect. -}
module Effect.CmdLine
    ( CmdLineM(..)
    , CommandLineOptions(..)
    ) where

import qualified System.Console.CmdArgs as CA

-- | Command line options.
data CommandLineOptions = CommandLineOptions
    { cmdLineConfigFile :: FilePath } deriving (Show, CA.Data, CA.Typeable, Eq)
{- HLINT ignore CommandLineOptions -}

-- | Command line effect.
class Monad m => CmdLineM m where
    -- | Parse command line options
    parseCommandLineOptions :: m CommandLineOptions

