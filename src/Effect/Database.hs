{- | Database effect. -}
module Effect.Database
    ( DatabaseM(..) ) where

import qualified Data.ByteString as BS

-- | Database effect.
class Monad m => DatabaseM c m | m -> c where
    connect :: c -> m () -- ^ Connect to database
    getByKey :: BS.ByteString -> m (Maybe BS.ByteString) -- ^ Retrieve value by key.
    setByKey :: BS.ByteString -> BS.ByteString -> m () -- ^ Set value by key.

