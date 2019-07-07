{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module CommonProperties where

import           ArbitraryInstances             ( )
import           Test.QuickCheck
import           Graphite.Types


range :: (Ord a, Arbitrary a) => Gen (a, a)
range = do
  x <- arbitrary
  y <- arbitrary `suchThat` (/= x)
  return (min x y, max x y)

daysFrom :: Word16 -> Time -> [Time]
daysFrom n = take (fromIntegral n + 1) . iterate (+ 86400)
