{-# LANGUAGE DeriveAnyClass #-}

module Normalisation
  ( rangeFrom
  , normalise
  , NormalisationFailure(..)
  )
where

import           Fmt
import           Data.List                      ( minimum
                                                , maximum
                                                )


newtype NormalisationFailure = NormalisationFailure {
  msg :: Text
} deriving (Eq, Show, Exception)

rangeFrom :: (Ord n) => [n] -> (n, n)
rangeFrom xs = (minimum xs, maximum xs)

normalise
  :: (Ord a, Fractional a, Show a)
  => (a, a)
  -> (a, a)
  -> a
  -> Either NormalisationFailure a
normalise to from i | invalidArgs = Left (NormalisationFailure buildFailureMsg)
                    | to == from  = Right i
                    | otherwise   = Right calc
 where
  invalidArgs     = (l == h) || (l' == h') || ((l > i) || (i > h))
  (l , h )        = to
  (l', h')        = from
  diffA           = h - l
  diffB           = h' - l'
  calc            = (((i - l) / diffA) * diffB) + l'
  buildFailureMsg = fmt
    ("Value: " +|| i ||+ " Origin: " +|| to ||+ " Target: " +|| from ||+ ".")
