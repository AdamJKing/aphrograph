module Normalisation
  ( rangeFrom
  , normalise
  )
where

import           Data.List                      ( minimum
                                                , maximum
                                                )


rangeFrom :: (Ord n) => [n] -> (n, n)
rangeFrom xs = (minimum xs, maximum xs)

normalise :: (Ord a, Fractional a, Show a) => (a, a) -> (a, a) -> a -> a
normalise to from i | to == from = i
                    | otherwise  = calc
 where
  (l , h ) = to
  (l', h') = from
  diffA    = h - l
  diffB    = h' - l'
  calc     = (((i - l) / diffA) * diffB) + l'
