{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Display.Labels where

import           Fmt
import           Data.Foldable                  ( maximum
                                                , minimum
                                                )


generateLabelsDiscrete
    :: (Show a, Integral a) => [a] -> (Int, Int) -> [(Int, Text)]
generateLabelsDiscrete input span =
    let noTicks = calcTickNum span
        offset  = minimum input
        spacer  = (maximum input - offset) `quot` fromIntegral noTicks
    in  [ (i * noTicks, show (offset + (fromIntegral i * spacer)))
        | i <- [0 .. noTicks]
        ]

generateLabelsContinuous
    :: (Show a, RealFrac a) => [a] -> (Int, Int) -> [(Int, Text)]
generateLabelsContinuous input span =
    let noTicks = calcTickNum span
        offset  = minimum input
        space   = (maximum input - offset) / fromIntegral noTicks
        labelValue i = showRounded (offset + (fromIntegral i * space))
    in  [ (i * noTicks, labelValue i) | i <- [0 .. noTicks] ]
    where showRounded = fmt . fixedF 4

calcTickNum :: (Int, Int) -> Int
calcTickNum = floor . sqrt . fromIntegral . delta
    where delta (mn, mx) = mx - mn
