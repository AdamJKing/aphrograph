{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module ArbitraryInstances where

import           App

import           DerivedArbitraryInstances      ()

import           Display.Graph                  as Graph
import           Display.Types

import           Graphics.Vty.Input.Events      as Vty

import           Graphite.Types                 as Graphite

import           System.Random

import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.ADT
import           Test.QuickCheck.Instances.Time ()

instance Arbitrary DataPoint where
    arbitrary = genericArbitrary

deriving instance (Generic n) => Generic (Dimensions n)

instance (Generic n, Arbitrary n) => Arbitrary (Dimensions n) where
    arbitrary = genericArbitrary

instance (Ord x, Arbitrary x, Ord y, Arbitrary y) => Arbitrary (Graph x y) where
    arbitrary = do
        (NonEmpty xs) <- arbitrary
        return $ Graph.mkGraph xs

data Range i = Range { lower :: i, higher :: i }
    deriving ( Show, Eq )

instance (Arbitrary i, Num i, Ord i) => Arbitrary (Range i) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary `suchThat` (/= a)
        return $ Range (min a b) (max a b)

instance Arbitrary AppState where
    arbitrary = applyArbitrary2 AppState

deriving via Text instance Arbitrary Graphite.From

deriving via Text instance Arbitrary Graphite.To

instance Arbitrary Text where
    arbitrary = fromString <$> arbitrary

instance Arbitrary ByteString where
    arbitrary = fromString <$> arbitrary

instance Arbitrary Vty.Modifier where
    arbitrary = genericArbitrary

deriving instance ToADTArbitrary Vty.Modifier

instance Arbitrary Vty.Key where
    arbitrary = genericArbitrary

deriving instance ToADTArbitrary Vty.Key

instance Arbitrary Vty.Button where
    arbitrary = genericArbitrary

instance Arbitrary Vty.Event where
    arbitrary = genericArbitrary

deriving instance ToADTArbitrary Vty.Event

instance Arbitrary Time where
    arbitrary = fromInteger <$> arbitrary

instance Random Time where
    randomR range = first fromInteger . randomR (toInts range)
      where
        toInts = join bimap (round . timestamp)

    random = randomR (0, 1)
