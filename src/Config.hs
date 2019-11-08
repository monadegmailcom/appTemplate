{- | Command line and config file parsing. -}
module Config
    ( Config(..)
    , Log(..)
    , Redis(..)
    , parseIniFile
    ) where

import           Config.Internal
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Ini as Ini
import qualified Data.Text as T

-- | Parse ini file semantically.
parseIniFile :: T.Text -> Either String (Config, T.Text)
parseIniFile content = do
    ini <- Ini.parseIni content
    config <- parseFromIni ini
    Right (config, Ini.printIni . redact $ ini)
  where
    redact = Ini.Ini . HashMap.adjust (HashMap.insert "auth" "<REDACTED>") "Redis" . Ini.unIni
