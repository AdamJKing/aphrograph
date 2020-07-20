{-# LANGUAGE ScopedTypeVariables #-}

module Display.ProjectionSpec where

import ArbitraryInstances
import CommonProperties
import Display.Projection.Scalable
import Formatting
import Graphite.Types
import Relude
import Test.Hspec as HS
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (scale)

spec :: HS.Spec
spec = describe "Projection" $ do
  describe "Scaled (Double to Integer)"
    . describe "scaling a value"
    . prop "scales to a value in the target range"
    $ \(from :: Range Value) (to :: Range Integer) -> forAll (choose (lower from, higher from)) $ \v ->
      let outcome = scale v (lower from, higher from) (lower to, higher to)
          desc = counterexample $ formatToString ("target value = " % shown % ", outcome was " % shown) v outcome
       in desc $ outcome `shouldSatisfy` (\a -> (lower to <= a) && a <= higher to)
  describe "Scaled (Time to Integer)" . describe "scaling a value" . prop "scales to a value in the target range" $ do
    (l, h) :: (Time, Time) <- range
    (l', h') :: (Int, Int) <- range
    v <- choose (l, h)
    let outcome = scale v (l, h) (l', h')
    return (outcome `shouldSatisfy` liftA2 (&&) (l' <=) (<= h'))
