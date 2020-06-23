{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module DerivedArbitraryInstances where

import App.Config as App
import Data.Decimal
import Display.Types
import GHC.Generics as GHC
import Graphite.Types as Graphite
import Network.HTTP.Req
  ( http,
    https,
  )
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.ADT
import Test.QuickCheck.Instances.Text ()
import Time.Types

newtype GenArbitrary a = GenArbitrary a deriving (Generic)

instance (Arbitrary a, GArbitrary (GHC.Rep a), Generic a) => Arbitrary (GenArbitrary a) where
  arbitrary = GenArbitrary <$> genericArbitrary

instance Arbitrary Natural where
  arbitrary = arbitrarySizedNatural

instance Arbitrary Dimensions where
  arbitrary = Dims <$> arbitrary <*> arbitrary

instance Arbitrary GraphiteUrl where
  arbitrary = oneof [return (GraphiteUrl (http "example.com")), return (GraphiteUrl (https "example.com"))]

--- --- --- Arbitrary --- --- ---
deriving instance Arbitrary Value

deriving instance Arbitrary Seconds

deriving via (GenArbitrary GraphiteConfig) instance Arbitrary App.GraphiteConfig

deriving instance Arbitrary App.Config

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
    let places = max (decimalPlaces a) (decimalPlaces b)
        a' = roundTo places a
        b' = roundTo places b
        (d, gen') = randomR (decimalMantissa a', decimalMantissa b') gen
     in (Decimal places d, gen')

  random = randomR (0, 1)
