module Display.Axis where

import           Normalisation
import           Data.Ord
import           Data.List
import           Labels
import           Graphite
import           Data.Hourglass

toVerticalAxis :: [DataPoint] -> Integer -> [Integer]
toVerticalAxis data' height = map
  (toInteger . normaliseIntegral toRange fromRange)
  points
 where
  points    = [ t | DataPoint { time = Elapsed (Seconds t) } <- data' ]
  toRange   = (0, fromInteger height)
  fromRange = rangeFrom points

toHorizontalAxis :: [DataPoint] -> Integer -> [Integer]
toHorizontalAxis data' height = map
  (round . normaliseFractional toRange fromRange)
  points
 where
  points    = [ v | DataPoint { value = v } <- data' ]
  toRange   = (0, fromInteger height)
  fromRange = rangeFrom points
