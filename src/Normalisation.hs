module Normalisation
  ( rangeFrom
  , normalise
  , NormalisationFailure(..)
  )
where

import           Data.List                      ( minimum
                                                , maximum
                                                )

data NormalisationFailure = BadOriginRange | BadTargetRange | BadValue deriving (Show, Eq)

rangeFrom :: (Ord n) => [n] -> (n, n)
rangeFrom xs = (minimum xs, maximum xs)

normalise
  :: (Ord a, Fractional a, Show a)
  => (a, a)
  -> (a, a)
  -> a
  -> Either NormalisationFailure a
normalise to from i | l == h             = Left BadOriginRange
                    | l' == h'           = Left BadTargetRange
                    | (l > i) || (i > h) = Left BadValue
                    | to == from         = Right i
                    | otherwise          = Right calc
 where
  (l , h ) = to
  (l', h') = from
  diffA    = h - l
  diffB    = h' - l'
  calc     = (((i - l) / diffA) * diffB) + l'
