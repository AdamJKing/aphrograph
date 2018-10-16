{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module ArbitraryInstances where

import           Data.Scientific
import qualified GraphiteSpec                   ( spec )
import           System.Random
import           Data.List                      ( sort )
import           Control.Lens                   ( over
                                                , _1
                                                )
import           Data.Int                       ( Int64 )
import           Control.Monad
import           Test.Hspec.Runner
import           Test.Hspec
import           Test.QuickCheck
import           Args
import           Time.Types
import           Test.QuickCheck.Gen
import           Data.Hourglass
import           Test.QuickCheck.Arbitrary
import           Normalisation

import           Graphite

instance Arbitrary Seconds where
    arbitrary = do
        Positive x <- arbitrary
        return $ Seconds x

    shrink (Seconds s) = Seconds <$> shrink s

deriving instance Arbitrary Elapsed

instance Arbitrary Scientific where
  arbitrary = scientific <$> arbitrary <*> arbitrary
  shrink = shrinkDecimal

instance Arbitrary DataPoint where
    arbitrary = DataPoint <$> arbitrary <*> arbitrary
    shrink DataPoint{value=v, time=t} = DataPoint <$> shrink v <*> shrink t

instance Random Elapsed where
    randomR (Elapsed (Seconds a), Elapsed (Seconds b)) = over _1 (Elapsed . Seconds) . randomR (a, b)
    random = over _1 (Elapsed . Seconds) . random

instance Arbitrary Dimensions where
    arbitrary = Dimensions <$> arbitrary <*> arbitrary
    shrink Dimensions{..} = Dimensions <$> shrink width `zip` shrink height

type Range a = (Positive a, Positive a)

generateRange :: (Ord a, Num a, Arbitrary a) => Gen (Range a)
generateRange = do
  Positive x <- arbitrary
  Positive y <- arbitrary
  return (Positive x, Positive (x + y))

genTestData
  :: (Arbitrary a, Random a, Ord a, Fractional a) => Gen (Range a, Positive a)
genTestData = do
  r1@(Positive i, Positive j) <- generateRange
  x                           <- choose (i, j)
  return (r1, Positive x)
