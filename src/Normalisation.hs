{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveAnyClass #-}

module Normalisation
  ( rangeFrom
  , normalise
  , NormalisationFailure(..)
  )
where

import           Fmt
import           Text.Show                     as Text
import           Data.List                      ( minimum
                                                , maximum
                                                )

data NormalisationFailure where
   BadOriginRange ::NormalisationFailure
   BadTargetRange ::NormalisationFailure
   BadValue ::(Show i) => i -> NormalisationFailure

deriving instance Exception NormalisationFailure

instance Text.Show NormalisationFailure where
  show BadOriginRange = "Invalid Origin Range"
  show BadTargetRange = "Invalid Target Range"
  show (BadValue i)   = "Invalid value: " +|| i ||+ ""

rangeFrom :: (Ord n) => [n] -> (n, n)
rangeFrom xs = (minimum xs, maximum xs)

normalise :: (Ord a, Fractional a, Show a) => (a, a) -> (a, a) -> a -> Either NormalisationFailure a
normalise to from i | l == h             = Left BadOriginRange
                    | l' == h'           = Left BadTargetRange
                    | (l > i) || (i > h) = Left (BadValue i)
                    | to == from         = Right i
                    | otherwise          = Right calc
 where
  (l , h ) = to
  (l', h') = from
  diffA    = h - l
  diffB    = h' - l'
  calc     = (((i - l) / diffA) * diffB) + l'
