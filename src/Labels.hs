{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}

module Labels where

import           Data.Decimal
import           Numeric.Natural
import           Normalisation
import           Graph                          ( normaliseRange )
import           UI.NCurses

accomodate :: (Integral a) => a -> a -> a
accomodate x d = if x `mod` d == 0 then x else accomodate (x + 1) d

createTruncatedLabel :: (Num a, Ord a, Show a) => a -> String
createTruncatedLabel value | value >= 1000  = take 1 $ show value ++ "k"
                           | value >= 10000 = take 2 $ show value ++ "m"
                           | otherwise      = show value

renderLabel :: (Integer, Integer) -> String -> Update ()
renderLabel (x, y) label = moveCursor y x >> drawString label

position :: Integer -> Integer -> [Integer]
position numberOfLabels targetSpace = [ x * i | i <- [1 .. numberOfLabels] ]
  where x = targetSpace `quot` numberOfLabels

generateLabels :: Decimal -> [String]
generateLabels value = createTruncatedLabel <$> allocate value [1..5]