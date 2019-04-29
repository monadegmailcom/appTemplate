{-| Implementation of the constraint based design with the help of multireader. -}
module MultiReader
    ( Constraint
    , (<:>)
    , ask
    , nil
    , runReader
    ) where

import           Control.Monad.HReader ( MonadHReader, MHRElemsConstraint, MHRElements, HReaderT
                                       , hask, runHReaderT)
import           Data.HSet (HGettable, HSet(..))
import qualified TypeFun.Data.List as TypeFun

-- | Define 'a' to be a constraint to 'm'.
type Constraint m a = MHRElemsConstraint m '[a]

-- | Ask a property from the multireader defined by the result type 'e'.
ask :: (MonadHReader m, HGettable (MHRElements m) e) => m e
ask = hask

-- | Transform the multireader to the monad 'm'.
runReader :: HSet els -> HReaderT els m a -> m a
runReader = runHReaderT

-- | Prepend a type to a type list.
(<:>) :: TypeFun.NotElem a as => a -> HSet as -> HSet (a : as)
(<:>) = HSCons

-- be right associative
infixr 1 <:>

-- | The empty type list.
nil :: HSet '[]
nil = HSNil

