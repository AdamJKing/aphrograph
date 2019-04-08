module Prelude
  ( module Relude
  , minMax
  )
where

import           Relude

minMax :: (Ord a) => NonEmpty a -> (a, a)
minMax (x :| []) = (x, x)
minMax (x :| xs) = (min mn x, max mx x) where (mn, mx) = minMax $ fromList xs
