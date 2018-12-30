{-# LANGUAGE ScopedTypeVariables #-}
module GraphiteSpec where

import           Test.Hspec                    as HS
import           Test.QuickCheck
import           Graphite
import           Data.List
import           Fmt
import           ArbitraryInstances             ( )


spec :: HS.Spec
spec =
  describe "Graphite"
    . describe "getValuesInTimeRange"
    . it "should detect values in the range"
    . property
    $ \(Ordered ts) -> (length ts > 1) ==> (ordNub ts == ts) ==> do
        p1 <- choose (0, length ts - 2)
        p2 <- choose (p1 + 1, length ts - 1)

        let DataPoint { time = a } = ts !! p1
        let DataPoint { time = b } = ts !! p2
        let real                   = getValuesInTimeRange (a, b) ts
        let expected               = take (p2 - p1 + 1) (drop p1 ts)
        let desc = counterexample $ "from (" +|| p1 ||+ ") to (" +|| p2 ||+ ")"

        return . desc $ real === expected
