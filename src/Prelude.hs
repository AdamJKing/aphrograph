module Prelude
  ( module Relude,
    module Relude.Extra.Newtype,
    module Relude.Extra.Type,
    module Control.Monad.Except,
    minMax,
    with,
  )
where

import Control.Lens.Getter
import Control.Monad.Except hiding
  ( fail,
    runExceptT,
  )
import GHC.TypeLits
import Relude
import Relude.Extra.Newtype
import Relude.Extra.Type

minMax :: (Ord a) => NonEmpty a -> (a, a)
minMax (x :| []) = (x, x)
minMax (x :| xs) = (min mn x, max mx x) where (mn, mx) = minMax $ fromList xs

with :: MonadReader s m => Getting t s t -> (t -> m b) -> m b
with lens f = view lens >>= f
