{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Labels where

import           Data.String
import           Data.Foldable                 as F
import           Data.Sequence                 as Seq
                                         hiding ( length )
import           Data.Decimal
import           Data.Monoid                    ( Sum(..)
                                                , getSum
                                                )
import           Data.Fixed                     ( mod' )

-- choosing the "unit" )is a different challenge

class LabellingStrategy a where
    generateLabels :: Int -> (a, a) -> [String]

    organiseLabels :: Int -> (a, a) -> String
    organiseLabels 0  _ = ""
    organiseLabels maxWidth range =
        let labels    = generateLabels maxWidth range
            labelSize = (maxWidth - F.length labels) `div` F.length labels
            space (fromList -> x)
                | F.length x > labelSize = error (fromString "Not enough space to display labels accurately " ++ show (F.length x) ++ " " ++ show labelSize)
                | otherwise =  ' ' :<| (emptyStringOf (labelSize - F.length x) >< x)
            in toList $ foldMap space labels

newtype DiscreteValue n = Discrete n deriving (Show, Eq, Num, Ord, Real, Enum, Integral)

instance LabellingStrategy Decimal where

    generateLabels _        (_    , 0    ) = return "0"
    generateLabels maxWidth (small, large) = toList $ findLabels 10
      where
        findLabels 0 = error "Cannot generate labels"
        findLabels n
            | (large - small) `mod'` i == 0
            = let
                  values =
                      fromFunction (n + 1) $ (small +) . (i *) . fromIntegral
                  largestDecimalPlace = maximum (decimalPlaces <$> values)
                  labels =
                      show
                          .   roundTo (fromIntegral largestDecimalPlace)
                          <$> values
                  fullLength = getSum $ foldMap (Sum . (+ 1) . length) labels
              in
                  if fullLength > maxWidth then findLabels (n - 1) else labels
            | otherwise
            = findLabels $ n - 1
            where i = (large - small) / fromIntegral n

instance (Show a, Enum a, Ord a, Integral a) => LabellingStrategy (DiscreteValue a) where

    generateLabels _        (_             , Discrete 0    ) = return "0"
    generateLabels maxWidth (Discrete small, Discrete large) = toList
        $ findLabels 10
      where
        findLabels 0 = error "Cannot generate labels"
        findLabels n
            | (large - small) `mod` fromIntegral n == 0
            = let
                  step = (large - small) `div` fromIntegral n
                  labels =
                      fromFunction (n + 1)
                          $ show
                          . (small +)
                          . (step *)
                          . fromIntegral
                  fullLength = getSum $ foldMap (Sum . (+ 1) . length) labels
              in
                  if fullLength > maxWidth then findLabels (n - 1) else labels
            | otherwise
            = findLabels $ n - 1

emptyStringOf :: Int -> Seq Char
emptyStringOf i = Seq.replicate i ' '

-- account for places < e

