{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Display.Labels where

import Data.Fixed (mod')
import Data.Foldable
  ( maximum,
    minimum,
  )
import Data.Time.LocalTime (TimeZone)
import Display.Projection.Scalable (Scalable (scale))
import Formatting (fixed, format, (%))
import Formatting.Time (dayOfMonth, month)
import qualified Formatting.Time as Format
import Graphite.Types
  ( Time,
    deltaDays,
    deltaHours,
    deltaMinutes,
    deltaSeconds,
    toLocalTime,
  )

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
          Second -> Format.hmsL
          _otherTimes -> Format.hm
      )

generateLabelsTime :: TimeZone -> [Time] -> (Int, Int) -> [(Int, LText)]
generateLabelsTime timezone times span = case nonEmpty times of
  Nothing -> []
  Just ns ->
    let (earliest, latest) = minMax ns
        step = determineStepSize (earliest, latest)
        stepTime = asTime step
        start = earliest + (stepTime - mod' earliest stepTime)
        steps = takeWhile (< latest) $ iterate (+ stepTime) start
     in [(scale i (earliest, latest) span, renderTimeLabel step timezone i) | i <- steps]
    where
      minMax :: Ord a => NonEmpty a -> (a, a)
      minMax (x :| []) = (x, x)
      minMax (x :| xs) = foldl' (\(mn, mx) n -> (min mn n, max mx n)) (x, x) xs

-- generateLabelsDiscrete :: (Show a, Integral a) => [a] -> (Int, Int) -> [(Int, LText)]
-- generateLabelsDiscrete input span =
--   let noTicks = calcTickNum span
--       offset = minimum input
--       spacer = (maximum input - offset) `quot` fromIntegral noTicks
--    in [(i * noTicks, show (offset + (fromIntegral i * spacer))) | i <- [0 .. noTicks]]

generateLabelsContinuous :: (Show a, RealFrac a) => [a] -> (Int, Int) -> [(Int, LText)]
generateLabelsContinuous input span = case nonEmpty input of
  Nothing -> []
  Just xs ->
    let noTicks = calcTickNum span
        offset = minimum xs
        space = (maximum xs - offset) / fromIntegral noTicks
        labelValue i = format (fixed 4) (offset + (fromIntegral i * space))
     in [(i * noTicks, labelValue i) | i <- [0 .. noTicks]]

calcTickNum :: (Int, Int) -> Int
calcTickNum (mn, mx) = let (totalSpan :: Double) = fromIntegral $ mx - fromIntegral mn in floor (sqrt totalSpan)
