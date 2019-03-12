{-# LANGUAGE ScopedTypeVariables #-}

module NormalisationSpec where

import           Normalisation
import           ArbitraryInstances
import           Test.QuickCheck
import           Test.Hspec.QuickCheck
import           Test.Hspec                    as HS

spec :: HS.Spec
spec = describe "normalise" $ do
    prop "has no effect when normalising to the same range" $ do
        Range { lower = l, higher = h } <- arbitrary :: Gen (Range Double)
        i                               <- choose (l, h)
        let outcome = normalise (l, h) (l, h) i
        return (i == outcome)

    prop
            "produces a value x times larger when the second range is x times larger"
        $ \(Positive f :: Positive Double, Range { lower = l, higher = h }) ->
              forAll (choose (l, h)) $ \v ->
                  let outcome = normalise (l, h) (l * f, h * f) v
                  in  (1e-3 > abs (outcome - (v * f)))

    prop
            "produces a value x times larger when the second range is x times smaller"
        $ \(Positive f :: Positive Double, Range { lower = l, higher = h }) ->
              forAll (choose (l, h)) $ \v ->
                  let outcome = normalise (l, h) (l / f, h / f) v
                  in  (1e-3 > abs (outcome - (v / f)))
