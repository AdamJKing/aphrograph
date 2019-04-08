{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module CommonProperties where

import           ArbitraryInstances             ( )
import           Test.QuickCheck
import           Graphite.Types


newtype UniqueList a = Unique { getUnique :: [a] }
  deriving (Show, Eq, Foldable)

instance (Arbitrary a, Eq a) => Arbitrary (UniqueList a) where
  arbitrary = sized $ fmap Unique . build
   where
    build 0  = return []
    build n' = do
      as <- build (n' - 1)
      a  <- arbitrary `suchThat` (`notElem` as)
      return $ a : as

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
