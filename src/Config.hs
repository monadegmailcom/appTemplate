{- | Command line and config file parsing. -}
module Config
    ( Config(..)
    , Destination(..)
    , Log(..)
    , Redis(..)
    , parseIniFile
    ) where

import           Config.Internal

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Ini as Ini
import qualified Data.Text as T

-- | Parse ini file semantically, return additionally a redacted printed config str.
parseIniFile :: T.Text -> Either String (Config, T.Text)
parseIniFile content = do
    ini <- Ini.parseIni content
    config <- parseFromIni ini
    Right (config, Ini.printIni . redact $ ini)
  where
    -- we do not want to expose confidential information like passwords
    redact ini =
        let adj = HashMap.adjust (HashMap.insert "auth" "<REDACTED>")
                                 "Redis"
                $ Ini.unIni ini
        in Ini.Ini (HashMap.map HashMap.toList adj) []
