{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Display.Labels where

import           Data.Decimal
import           Data.Foldable                  ( maximum
                                                , minimum
                                                )


generateLabelsDiscrete
    :: (Show a, Integral a) => [a] -> (Int, Int) -> [(Int, Text)]
generateLabelsDiscrete input (mn, mx) =
    let noTicks = floor (sqrt (fromIntegral (mx - mn)))
        spacer  = (maximum input - minimum input) `quot` fromIntegral noTicks
    in  [ (i * noTicks, show (fromIntegral i * spacer)) | i <- [0 .. noTicks] ]

generateLabelsContinuous
    :: (Show a, RealFrac a) => [a] -> (Int, Int) -> [(Int, Text)]
generateLabelsContinuous input (mn, mx) =
    let noTicks = floor (sqrt (fromIntegral (mx - mn)))
        spacer  = (maximum input - minimum input) / fromIntegral noTicks
    in  [ (i * noTicks, show (realFracToDecimal 4 $ fromIntegral i * spacer))
        | i <- [0 .. noTicks]
        ]

