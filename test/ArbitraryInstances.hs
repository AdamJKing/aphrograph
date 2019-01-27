{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ArbitraryInstances where

import           Display.Graph                 as Graph
import           Test.QuickCheck
import           DerivedArbitraryInstances      ( )
import           Test.QuickCheck.Arbitrary.ADT
import           Display.Types
import           Graphite


instance Arbitrary DataPoint where
    arbitrary = genericArbitrary

deriving instance (Generic n) => Generic (Dimensions n)

instance (Generic n, Arbitrary n) => Arbitrary (Dimensions n) where
    arbitrary = genericArbitrary

instance (Ord x, Arbitrary x, Ord y, Arbitrary y) => Arbitrary (Graph x y) where
    arbitrary = do
        (NonEmpty xs) <- arbitrary
        return $ Graph.mkGraph xs

data Range i = Range {
    lower :: i,
    higher :: i
} deriving (Show, Eq)

instance (Arbitrary i, Num i, Ord i) => Arbitrary (Range i) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary `suchThat` (/= a)
        return $ Range (min a b) (max a b)
