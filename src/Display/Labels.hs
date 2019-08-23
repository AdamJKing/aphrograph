{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Display.Labels where

import           Fmt
import           Data.Foldable                  ( maximum
                                                , minimum
                                                )
import           Graphite.Types
import           Data.Time.LocalTime
import           Data.Fixed
import           Display.Projection.Scalable


generateSteps :: (Time, Time) -> [Time]
generateSteps (earliest, latest) =
    let step = asTime $ determineStepSize (earliest, latest)
    in  [earliest, earliest + step .. latest]

data TimeStep = Day | Hour | FiveMinute | Minute | Second | Millisecond
  deriving ( Show , Eq , Generic, Enum )

asTime :: TimeStep -> Time
asTime Day         = 86400
asTime Hour        = 3600
asTime FiveMinute  = 300
asTime Minute      = 60
asTime Second      = 1
asTime Millisecond = 0.001

determineStepSize :: (Time, Time) -> TimeStep
determineStepSize (earliest, latest)
    | deltaDays earliest latest > 1    = Day
    | deltaHours earliest latest > 1   = Hour
    | deltaMinutes earliest latest > 5 = FiveMinute
    | deltaMinutes earliest latest > 0 = Minute
    | deltaSeconds earliest latest > 0 = Second
    | otherwise                        = Millisecond

renderTimeLabel :: TimeStep -> TimeZone -> Time -> Text
renderTimeLabel step timezone = fmt . renderFunc . toLocalTime timezone
  where
    renderFunc = case step of
        Day -> timeF "%d/%m"
        _   -> timeF "%H:%M"


generateLabelsTime :: TimeZone -> [Time] -> (Int, Int) -> [(Int, Text)]
generateLabelsTime _ [] _ = []
generateLabelsTime timezone times span =
    let (earliest, latest) = minMax (fromList times)
        step               = determineStepSize (earliest, latest)
        stepTime           = asTime step
        start              = earliest + (stepTime - mod' earliest stepTime)
        steps              = takeWhile (< latest) $ iterate (+ stepTime) start
    in  [ (scale i (earliest, latest) span, renderTimeLabel step timezone i)
        | i <- steps
        ]

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
