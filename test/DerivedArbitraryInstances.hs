{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module DerivedArbitraryInstances where

import           System.Random
import           Data.Decimal
import           Test.QuickCheck
import           Time.Types
import           Graphite.Types                as Graphite
import           Test.QuickCheck.Instances.Text ( )
import           Test.QuickCheck.Arbitrary.ADT
import           GHC.Generics                  as GHC

newtype GenArbitrary a = GenArbitrary a deriving Generic

instance (Arbitrary a, GArbitrary (GHC.Rep a), Generic a) => Arbitrary (GenArbitrary a) where
  arbitrary = GenArbitrary <$> genericArbitrary

--- --- --- Arbitrary --- --- ---
deriving instance Arbitrary Value
deriving instance Arbitrary Seconds

deriving via Text instance Arbitrary Metric
deriving via Text instance Arbitrary Graphite.From
deriving via Text instance Arbitrary Graphite.To

--- --- ---  Random   --- --- ---
deriving instance Random Seconds
deriving instance Random Elapsed
deriving instance Random Value
deriving via (Positive Int64) instance Arbitrary Elapsed

--- --- ---  Generic  --- --- ---
deriving instance Generic DataPoint

instance (Arbitrary i) => Arbitrary (DecimalRaw i) where
  arbitrary = applyArbitrary2 Decimal

instance (Random i, Integral i, Show i) => Random (DecimalRaw i) where
  randomR (a, b) gen =
    let places    = max (decimalPlaces a) (decimalPlaces b)
        a'        = roundTo places a
        b'        = roundTo places b
        (d, gen') = randomR (decimalMantissa a', decimalMantissa b') gen
    in  (Decimal places d, gen')

  random = randomR (0, 1)
