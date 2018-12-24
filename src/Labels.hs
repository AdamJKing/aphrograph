{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Labels where

import qualified Data.Foldable                 as F
import           Data.Decimal
import           Data.Fixed                     ( mod' )
import qualified Data.Text                     as T
import qualified Fmt

-- choosing the "unit" is a different challenge
data LabellingError = DisplayTooSmall deriving (Eq, Show)

organiseLabels :: Int -> [Text] -> Either LabellingError Text
organiseLabels maxWidth labels
    | maxWidth < largestLabelSize
    = Left DisplayTooSmall
    | otherwise
    = let labelSize = maxWidth `div` length labels
          space x | T.length x > labelSize = Left DisplayTooSmall
                  | otherwise = Right $ Fmt.padLeftF labelSize ' ' x
      in  if labelSize >= largestLabelSize
              then Fmt.fmt . fold <$> traverse space labels
              else Left DisplayTooSmall
    where largestLabelSize = F.maximum (T.length <$> labels)

class LabellingStrategy a where
    generateLabels :: Int -> (a, a) -> [Text]

newtype DiscreteValue n = Discrete n deriving (Show, Eq, Num, Ord, Real, Enum, Integral)

instance LabellingStrategy Decimal where

    generateLabels _        (_    , 0    ) = return "0"
    generateLabels maxWidth (small, large) = findLabels 10
      where
        findLabels 0 = error "Cannot generate labels"
        findLabels n
            | (large - small) `mod'` i == 0
            = let
                  values              = [ small + (i * g) | g <- [0 .. n + 1] ]
                  largestDecimalPlace = F.maximum (decimalPlaces <$> values)
                  labels =
                      show
                          .   roundTo (fromIntegral largestDecimalPlace)
                          <$> values
                  fullLength = getSum $ foldMap (Sum . (+ 1) . T.length) labels
              in
                  if fullLength > maxWidth then findLabels (n - 1) else labels
            | otherwise
            = findLabels $ n - 1
            where i = (large - small) / n

instance (Show a, Enum a, Ord a, Integral a) => LabellingStrategy (DiscreteValue a) where

    generateLabels 0        _ = return ""
    generateLabels _ (_, Discrete 0) = return "0"
    generateLabels maxWidth (Discrete small, Discrete large) = findLabels 10
      where
        findLabels 0 = error "Cannot generate labels"
        findLabels n
            | (large - small) `mod` fromIntegral n == 0
            = let step       = (large - small) `div` fromIntegral n
                  labels     = [ show (small + (step * g)) | g <- [0 .. n] ]
                  fullLength = getSum $ foldMap (Sum . (+ 1) . T.length) labels
              in  if fullLength > maxWidth then findLabels (n - 1) else labels
            | otherwise
            = findLabels $ n - 1

-- account for places < e

