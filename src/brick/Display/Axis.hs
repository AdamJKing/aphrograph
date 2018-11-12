module Display.Axis where

-- data Axis = VerticalAxis | HorizontalAxis

-- class AxisTransformer axis 

-- toVerticalAxis :: [DataPoint] -> Integer -> [Integer]
-- toVerticalAxis data' _height = map
--   (toInteger . normaliseIntegral toRange fromRange)
--   points
--  where
--   points    = [ t | DataPoint { time = Elapsed (Seconds t) } <- data' ]
--   toRange   = (0, fromInteger _height)
--   fromRange = rangeFrom points

-- toHorizontalAxis :: [DataPoint] -> Integer -> [Integer]
-- toHorizontalAxis data' _height = map
--   (round . normaliseFractional toRange fromRange)
--   points
--  where
--   points    = [ v | DataPoint { value = v } <- data' ]
--   toRange   = (0, fromInteger _height)
--   fromRange = rangeFrom points
