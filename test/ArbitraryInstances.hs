{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ArbitraryInstances where

import           Labels
import           Data.Decimal
import           Display.Graph                 as Graph
import           System.Random
import           Test.QuickCheck

import           Test.QuickCheck.Arbitrary.ADT
import           Display.Types
import           Time.Types
import           Graphite

deriving instance Arbitrary Time
deriving via Int64 instance Arbitrary Elapsed

newtype TestDiscreteValue = TestDiscrete (DiscreteValue Int) deriving (Show, Eq)

instance Arbitrary TestDiscreteValue where
    arbitrary = TestDiscrete . Discrete <$> arbitrary

newtype NonZeroTestDiscreteValue = NonZeroTestDiscrete (DiscreteValue Int) deriving (Show, Eq)

instance Arbitrary NonZeroTestDiscreteValue where
    arbitrary = NonZeroTestDiscrete . Discrete <$> (arbitrary `suchThat` (> 0))

newtype TestDecimal = TestDecimal Decimal deriving (Show, Eq, Num)

instance Arbitrary TestDecimal where
    arbitrary = do
        (Positive a) <- arbitrary
        b            <- choose (0, 10)
        return . TestDecimal $ Decimal a b


newtype NonZeroTestDecimal = NonZeroTestDecimal Decimal deriving (Show, Eq)

instance Arbitrary NonZeroTestDecimal where
    arbitrary = do
        (NonZero (TestDecimal d)) <- arbitrary
        return . NonZeroTestDecimal $ d

deriving instance Arbitrary Value

deriving instance Generic DataPoint

instance Arbitrary DataPoint where
    arbitrary = genericArbitrary

deriving instance (Generic n) => Generic (Dimensions n)

instance (Generic n, Arbitrary n) => Arbitrary (Dimensions n) where
    arbitrary = genericArbitrary

deriving instance Random Seconds
deriving instance Random Elapsed
deriving instance Random Time
deriving instance Random Value

instance (Ord x, Arbitrary x, Ord y, Arbitrary y) => Arbitrary (Graph x y) where
    arbitrary = do
        (NonEmpty xs) <- arbitrary
        return $ Graph.mkGraph xs

data Range i = Range {
    lower :: i,
    higher :: i
} deriving (Show, Eq)

tuple :: Range n -> (n, n)
tuple Range {..} = (lower, higher)

instance (Ord a, Arbitrary a) => Arbitrary (Range a) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary `suchThat` (/=) x
        return $ Range { lower = min x y, higher = max x y }

genTestData :: (Arbitrary a, Random a, Ord a, Num a) => Gen (Range a, Positive a)
genTestData = do
    r@Range {..} <- arbitrary
    x            <- choose (lower, higher)
    return (r, Positive x)
