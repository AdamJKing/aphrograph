{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import           Data.Decimal
import           ErrorHandlingSpec
import           System.Random
import           Data.List                      ( sort )
import           Control.Lens                   ( over
                                                , _1
                                                )
import           Data.Int                       ( Int64 )
import           Control.Monad
import           Test.Hspec.Runner
import           Test.Hspec
import           Test.QuickCheck
import           Args
import           Time.Types
import           Test.QuickCheck.Gen
import           Data.Hourglass
import           Test.QuickCheck.Arbitrary

import           Graphite
import           Graph

instance Arbitrary Seconds where
    arbitrary = do
        Positive x <- arbitrary
        return $ Seconds x

    shrink (Seconds s) = Seconds <$> shrink s

deriving instance Arbitrary Elapsed

instance Arbitrary Decimal where
  arbitrary = Decimal <$> arbitrary <*> arbitrary

instance Arbitrary DataPoint where
    arbitrary = DataPoint <$> arbitrary <*> arbitrary
    shrink DataPoint{value=v, time=t} = DataPoint <$> shrink v <*> shrink t

instance Random Elapsed where
    randomR (Elapsed (Seconds a), Elapsed (Seconds b)) = over _1 (Elapsed . Seconds) . randomR (a, b)
    random = over _1 (Elapsed . Seconds) . random

type Range a = (Positive a, Positive a)

generateRange :: (Ord a, Num a, Arbitrary a) => Gen (Range a)
generateRange = do
  Positive x <- arbitrary
  Positive y <- arbitrary
  return (Positive x, Positive (x + y))

genTestData
  :: (Arbitrary a, Random a, Ord a, Fractional a) => Gen (Range a, Positive a)
genTestData = do
  r1@(Positive i, Positive j) <- generateRange
  x                           <- choose (i, j)
  return (r1, Positive x)

main :: IO ()
main = hspec $ do
  describe "Error Handling" ErrorHandlingSpec.spec
  describe "Graphite.getValuesInTimeRange"
    $ it "should detect values in the range"
    $ forAll
        arbitrary
        (\(Ordered ts) -> do
          a <- choose (time . last $ ts, time . head $ ts)
          b <- choose (a, time (head ts))
          return
            . all (\DataPoint { time = t } -> a <= t && t <= b)
            $ getValuesInTimeRange (a, b) ts
        )
  describe "Graph.normalise" $ do
    it "has no effect when normalising to the same range"
      . forAll (genTestData :: Gen (Range Double, Positive Double))
      $ \((Positive a, Positive b), Positive i) ->
          normalise (a, b) (a, b) i === i
    it "produces a value x times larger when the second range is x times larger"
      . forAll (genTestData :: Gen (Range Double, Positive Double))
      $ \((Positive a, Positive b), Positive v) -> do
          Positive f <- resize 500 $ arbitrary :: Gen (Positive Double)
          return $ (floor $ normalise (a, b) (a * f, b * f) v) === floor (v * f)
  describe "Main.parseTime" $ do
    it "parses seconds" $ (parseTime "30s") === Just (Seconds 30)
    it "parses minutes" $ (parseTime "60m") === Just (Seconds 3600)
    it "parses seconds" $ (parseTime "24h") === Just (Seconds 86400)
    it "parses seconds" $ (parseTime "7d") === Just (Seconds 604800)
    it "returns left on errors" $ (parseTime "gibberish") === Nothing
