module Axis
  ( verticalAxisTransformer
  , horizontalAxisTransformer
  , renderVerticalAxis
  , renderHorizontalAxis
  )
where

import           Data.Ord
import           Data.List
import qualified UI.NCurses                    as NC
import           Data.Decimal
import           Labels
import           Graphite
import           Data.Hourglass

verticalAxisTransformer :: [DataPoint] -> Decimal
verticalAxisTransformer = value . maximumBy (comparing value)

-- todo: problem, defaulting to a unit! assuming seconds
horizontalAxisTransformer :: [DataPoint] -> Decimal
horizontalAxisTransformer = toDecimal . time . maximum
 where
  toDecimal (Elapsed n) =
    Decimal {decimalMantissa = toInteger n, decimalPlaces = 0}

renderVerticalAxis :: Decimal -> NC.Update ()
renderVerticalAxis displayData = do
  (width, height) <- NC.windowSize
  let labels    = generateLabels displayData
  let positions = position (toInteger $ length labels) height
  let xPos      = width `quot` 2
  mapM_ (\(label, yPos) -> renderLabel (xPos, yPos) label)
        (labels `zip` positions)

renderHorizontalAxis :: Decimal -> NC.Update ()
renderHorizontalAxis displayData = do
  (width, height) <- NC.windowSize
  let labels    = generateLabels displayData
  let positions = position (toInteger $ length labels) width
  let yPos      = height `quot` 2
  mapM_ (\(label, xPos) -> renderLabel (xPos, yPos) label)
        (labels `zip` positions)
