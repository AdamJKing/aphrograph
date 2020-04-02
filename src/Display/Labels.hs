{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Display.Labels where

import Data.Fixed
import Data.Foldable
  ( maximum,
    minimum,
  )
import Data.Time.LocalTime
import Display.Projection.Scalable
import Formatting
import Formatting.Time
import Graphite.Types

data TimeStep = Day | Hour | FiveMinute | Minute | Second | Millisecond
  deriving (Show, Eq, Generic, Enum)

asTime :: TimeStep -> Time
asTime Day = 86400
asTime Hour = 3600
asTime FiveMinute = 300
asTime Minute = 60
asTime Second = 1
asTime Millisecond = 0.001

determineStepSize :: (Time, Time) -> TimeStep
determineStepSize (earliest, latest)
  | deltaDays earliest latest > 1 = Day
  | deltaHours earliest latest > 1 = Hour
  | deltaMinutes earliest latest > 5 = FiveMinute
  | deltaMinutes earliest latest > 0 = Minute
  | deltaSeconds earliest latest > 0 = Second
  | otherwise = Millisecond

renderTimeLabel :: TimeStep -> TimeZone -> Time -> LText
renderTimeLabel step timezone time =
  toLocalTime timezone time
    & format
      ( case step of
          Day -> dayOfMonth <> "/" % month
          _ -> hmsL
      )

generateLabelsTime :: TimeZone -> [Time] -> (Int, Int) -> [(Int, LText)]
generateLabelsTime _ [] _ = []
generateLabelsTime timezone times span =
  let (earliest, latest) = minMax (fromList times)
      step = determineStepSize (earliest, latest)
      stepTime = asTime step
      start = earliest + (stepTime - mod' earliest stepTime)
      steps = takeWhile (< latest) $ iterate (+ stepTime) start
   in [(scale i (earliest, latest) span, renderTimeLabel step timezone i) | i <- steps]

-- generateLabelsDiscrete :: (Show a, Integral a) => [a] -> (Int, Int) -> [(Int, LText)]
-- generateLabelsDiscrete input span =
--   let noTicks = calcTickNum span
--       offset = minimum input
--       spacer = (maximum input - offset) `quot` fromIntegral noTicks
--    in [(i * noTicks, show (offset + (fromIntegral i * spacer))) | i <- [0 .. noTicks]]

generateLabelsContinuous :: (Show a, RealFrac a) => [a] -> (Int, Int) -> [(Int, LText)]
generateLabelsContinuous [] _ = []
generateLabelsContinuous input span =
  let noTicks = calcTickNum span
      offset = minimum input
      space = (maximum input - offset) / fromIntegral noTicks
      labelValue i = format (fixed 4) (offset + (fromIntegral i * space))
   in [(i * noTicks, labelValue i) | i <- [0 .. noTicks]]

calcTickNum :: (Int, Int) -> Int
calcTickNum (mn, mx) = let (totalSpan :: Double) = fromIntegral $ mx - fromIntegral mn in floor (sqrt totalSpan)
