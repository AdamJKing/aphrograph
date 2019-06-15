{-# LANGUAGE ScopedTypeVariables #-}

module Display.ProjectionSpec where

import           Fmt
import           CommonProperties
import           Test.Hspec                    as HS
import           Test.Hspec.QuickCheck
import           Test.QuickCheck         hiding ( scale )
import           Display.Projection.Scalable
import           Graphite.Types
import           ArbitraryInstances


spec :: HS.Spec
spec = describe "Projection" $ do
    describe "Scaled (Double to Integer)"
        . describe "scaling a value"
        . prop "scales to a value in the target range"
        $ \(from :: Range Value) (to :: Range Integer) ->
              forAll (choose (lower from, higher from)) $ \v ->
                  let
                      outcome = scale v
                                      (lower from, higher from)
                                      (lower to  , higher to)
                      desc =
                          counterexample
                              $   "target value = "
                              +|| v
                              ||+ ", outcome was "
                              +|| outcome
                              ||+ ""
                  in
                      desc
                      $               outcome
                      `shouldSatisfy` (\a -> (lower to <= a) && a <= higher to)

    describe "Scaled (Time to Integer)"
        . describe "scaling a value"
        . prop "scales to a value in the target range"
        $ do
              (l , h ) :: (Time, Time) <- range
              (l', h') :: (Int, Int)   <- range

              v                        <- choose (l, h)

              let outcome = scale v (l, h) (l', h')
              return (outcome `shouldSatisfy` liftA2 (&&) (l' <=) (<= h'))
