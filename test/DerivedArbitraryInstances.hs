{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module DerivedArbitraryInstances where

import           System.Random
import           Data.Decimal
import           Test.QuickCheck
import           Time.Types
import           Graphite


--- --- --- Arbitrary --- --- ---
deriving instance Arbitrary Value
deriving instance Arbitrary Time

--- --- ---  Random   --- --- ---
deriving instance Random Seconds
deriving instance Random Elapsed
deriving instance Random Time
deriving instance Random Value
deriving via Int64 instance Arbitrary Elapsed

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