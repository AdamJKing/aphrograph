{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import           Data.List                     as List
import           Test.Hspec.Runner
import           Test.Hspec
import           Test.QuickCheck
import           ArbitraryInstances
import           Graphite
import qualified GraphSpec                      ( spec )
import qualified GraphiteSpec                   ( spec )
import qualified ArgsSpec                       ( spec )
import qualified NormalisationSpec              ( spec )

main :: IO ()
main = hspec $ do
  describe "Graph"         GraphSpec.spec
  describe "Graphite"      GraphiteSpec.spec
  describe "Args"          ArgsSpec.spec
  describe "Normalisation" NormalisationSpec.spec
  describe "Graphite.getValuesInTimeRange"
    $ it "should detect values in the range"
    $ forAll
        (          arbitrary
        `suchThat` (\(Ordered ns) -> nub ns == ns)
        `suchThat` (\(Ordered ns) -> not (null ns) && (length ns > 1))
        )
    $ \(Ordered ts) -> do
        let ots = List.sort ts
        p1 <- choose (0, length ts - 2)
        p2 <- choose (p1 + 1, length ts - 1)
        let (SimpleDataPoint a) = ots !! p1
        let (SimpleDataPoint b) = ots !! p2
        let ts'                 = (\(SimpleDataPoint dp) -> dp) <$> ts
        let real = getValuesInTimeRange (time a, time b) ts'
        let expected = (\(SimpleDataPoint dp) -> dp)
              <$> take (p2 - p1 + 1) (drop p1 ts)
        return
          .   counterexample ("from (" ++ show p1 ++ ") to (" ++ show p2 ++ ")")
          $   real
          === expected
