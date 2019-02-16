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
              assertLeft
                  $ normalise (sameValue, sameValue) (lower, higher) value

    it "rejects bad target ranges"
        . property
        $ \(sameValue :: Double) (Range {..} :: Range Double) (value :: Double) ->
              assertLeft
                  $ normalise (lower, higher) (sameValue, sameValue) value

    it "rejects bad values" . property $ \(Range l h) ->
        let isOutOfRange i = i > h || i < l
        in
            do
                (Range l' h') <- arbitrary :: Gen (Range Double)
                i             <- arbitrary `suchThat` isOutOfRange
                return . counterexample (show i) . assertLeft $ normalise
                    (l , h)
                    (l', h')
                    i

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
  where
    assertLeft op = case op of
        Right _ -> False
        Left  _ -> True

