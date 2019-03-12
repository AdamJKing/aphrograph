{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Display.Labels where

import           Data.Foldable                  ( maximum
                                                , minimum
                                                )

data Label = Label !Text !Int deriving (Eq, Show)

label :: Label -> Text
label (Label txt _) = txt

position :: Label -> Int
position (Label _ pos) = pos

generateLabelsDiscrete :: (Show a, Integral a) => [a] -> (Int, Int) -> [Label]
generateLabelsDiscrete input (mn, mx) =
    let noTicks = floor (sqrt (fromIntegral (mx - mn)))
        spacer  = (maximum input - minimum input) `quot` fromIntegral noTicks
    in  [ Label (show (fromIntegral i * spacer)) (i * noTicks)
        | i <- [0 .. noTicks]
        ]

generateLabelsContinuous :: (Show a, RealFrac a) => [a] -> (Int, Int) -> [Label]
generateLabelsContinuous input (mn, mx) =
    let noTicks = floor (sqrt (fromIntegral (mx - mn)))
        spacer  = (maximum input - minimum input) / fromIntegral noTicks
    in  [ Label (show (fromIntegral i * spacer)) (i * noTicks)
        | i <- [0 .. noTicks]
        ]

-- generateLabels' :: ProjectionContext Double Int -> [Label Double]
-- generateLabels' ctxt = mkLabel <$> takeWhile (< largest) [0 ..]
--  where
--   diff (a, b) = b - a
--   span                = diff (projection ctxt)
--   (smallest, largest) = origin ctxt
--   noTicks             = 0.3 * sqrt (fromIntegral span)
--   tickSize            = computeTickSize smallest largest noTicks
--   start               = tickSize * floor' (smallest / tickSize)
--   mkLabel i = Label (show (start + (i * tickSize))) (round i)

-- computeTickSize :: Double -> Double -> Double -> Double
-- computeTickSize mn mx noTicks =
--   let delta = (mx - mn) / noTicks
--       dec   = -floor (logBase 10 delta) :: Int
--       magn  = 10 ^^ (-dec)
--       norm  = delta / magn
--   in  if
--         | norm < 1.5 -> 1
--         | norm < 3   -> 2
--         | norm < 7.5 -> 5
--         | otherwise  -> 10


    -- do
    -- originSpan <- reader (snd . origin)
    -- targetSpan <- reader (snd . projection)

    -- let spacer = calculateSpacer (fromIntegral originSpan ) targetSpan
    -- let ticks  = fromIntegral targetSpan / spacer

    -- forM [0 .. ticks] $ \pos -> do
    --     v <- original (pos * spacer)
    --     return (Label (show v) (round (pos * spacer)))

    -- where
    -- calculateSpacer :: (Integral a) => a -> a -> Double
    -- calculateSpacer a b = sqrt . fromIntegral $ lcm a b

    -- original v =
    --     withReaderT (first toDouble . reverseProjection) (project v)

    -- toDouble :: (Integral a) => a -> Double
    -- toDouble = fromIntegral
