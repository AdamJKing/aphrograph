{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NormalisationSpec where

import           Normalisation
import           Test.QuickCheck
import           Test.Hspec.QuickCheck
import           Test.Hspec                    as HS
import           CommonProperties

factor :: Gen Double
factor = getPositive <$> arbitrary

spec :: HS.Spec
spec = describe "normalise" $ do
    prop "has no effect when normalising to the same range" $ do
        (l, h) <- range @Double
        i      <- choose (l, h)
        return (i === normalise (l, h) (l, h) i)

    prop
            "a range that is linearly smaller/bigger by a factor f is the same as doing ( f * that value )"
        $ do
              f      <- factor
              (l, h) <- range
              v      <- choose (l, h)
              let outcome = normalise (l, h) (l * f, h * f) v
              return (1e-3 > abs (outcome - (v * f)))

    prop
            "if the target range has no actual range than the normalised value is that number (ie normalising to (10, 10) always results in 10)"
        $ do
              origin <- range @Double
              v      <- choose origin
              a      <- arbitrary
              return (a === normalise origin (a, a) v)

    prop
            "if the origin has no range, then there is an error because it's impossible to determine what it should be the in the target range"
        $ do
              target <- range @Double
              v      <- choose target
              a      <- arbitrary
              let
                  outcome =
                      evaluateNF (normalise (a, a) target v)
                          `shouldThrow` anyErrorCall
              return outcome

    prop
            "if the origin has no range, then there is an error because it's impossible to determine what it should be the in the target range"
        $ do
              (lowEnd, highEnd) <- range @Double
              target <- range @Double
              value <- arbitrary `suchThat` \n -> lowEnd > n || highEnd < n
              let outcome =
                      evaluateNF (normalise (lowEnd, highEnd) target value)
                          `shouldThrow` anyErrorCall
              return outcome
