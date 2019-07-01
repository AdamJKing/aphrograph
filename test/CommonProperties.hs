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

-- Temporary until QuickCheck 2.13
instance Testable prop => Testable (Maybe prop) where
  property = property . liftMaybe
   where
    liftMaybe Nothing     = property Discard
    liftMaybe (Just prop) = property prop

daysFrom :: Word16 -> Time -> [Time]
daysFrom n = take (fromIntegral n + 1) . iterate (+ 86400)
