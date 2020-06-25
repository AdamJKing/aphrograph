{-# LANGUAGE TypeFamilies #-}

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
import Control.Monad.Trans.Identity
import Data.Semigroup
import GHC.TypeLits
import Relude
import Relude.Extra.Foldable1
import Relude.Extra.Newtype
import Relude.Extra.Type

minMax :: Ord a => NonEmpty a -> (a, a)
minMax xs = let (Min mn, Max mx) = foldMap1 (\n -> (Min n, Max n)) xs in (mn, mx)

with :: MonadReader s m => Getting t s t -> (t -> m b) -> m b
with lens f = view lens >>= f
