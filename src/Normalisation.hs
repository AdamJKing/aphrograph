module Normalisation
  ( rangeFrom
  , normalise
  )
where

import           Data.List                      ( minimum
                                                , maximum
                                                )

import Fmt

rangeFrom :: (Ord n) => [n] -> (n, n)
rangeFrom xs = (minimum xs, maximum xs)

normalise :: (Ord a, Fractional a, Show a) => (a, a) -> (a, a) -> a -> a
normalise _ (0, 0) _ = 0
normalise from @ (l, h) to @ (l', h') i 
  | to == from = i
  | l == h = noBaseRangeErr
  | otherwise  = calc
 where
  diffA    = h - l
  diffB    = h' - l'
  calc     = (((i - l) / diffA) * diffB) + l' 
  noBaseRangeErr = error $ "Cannot normalise without a concept of a base range (" +|| from ||+ ")."