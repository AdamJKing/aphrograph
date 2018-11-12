{-# LANGUAGE ScopedTypeVariables #-}

module NormalisationSpec where

import           CommonProperties
import           Normalisation
import           ArbitraryInstances
import           Test.QuickCheck
import           Test.Hspec                    as HS

spec :: HS.Spec
spec = describe "normalise" $ do
    it "rejects bad origin ranges"
        . property
        $ \(sameValue :: Double) (Range {..} :: Range Double) (value :: Double) ->
              normalise (sameValue, sameValue) (lower, higher) value
                  === Left BadOriginRange

    it "rejects bad target ranges"
        . property
        $ \(sameValue :: Double) (Range {..} :: Range Double) (value :: Double) ->
              normalise (lower, higher) (sameValue, sameValue) value
                  === Left BadTargetRange

    it "rejects bad values"
        $ forAll (arbitrary :: Gen (Range Double))
        $ \rng ->
              let isOutOfRange i = i > higher rng || i < lower rng
              in  do
                      ignored <- arbitrary :: Gen (Range Double)
                      i       <- arbitrary `suchThat` isOutOfRange
                      return
                          .   counterexample (show i)
                          $   normalise (tuple rng) (tuple ignored) i
                          === Left BadValue

    it "has no effect when normalising to the same range" . property $ do
        Range { lower = l, higher = h } <- arbitrary :: Gen (Range Double)
        i                               <- choose (l, h)
        let outcome = normalise (l, h) (l, h) i
        return $ ofEither (== i) outcome

    it "produces a value x times larger when the second range is x times larger"
        . property
        $ \(Positive f :: Positive Double, Range { lower = l, higher = h }) ->
              forAll (choose (l, h)) $ \v ->
                  let outcome = normalise (l, h) (l * f, h * f) v
                  in  ofEither (\i -> 1e-3 > abs (i - (v * f))) outcome

    it
            "produces a value x times larger when the second range is x times smaller"
        . property
        $ \(Positive f :: Positive Double, Range { lower = l, higher = h }) ->
              forAll (choose (l, h)) $ \v ->
                  let outcome = normalise (l, h) (l / f, h / f) v
                  in  ofEither (\i -> 1e-3 > abs (i - (v / f))) outcome
