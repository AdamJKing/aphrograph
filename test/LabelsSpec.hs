{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LabelsSpec where

import CommonProperties
import Data.Decimal
import Data.Fixed
import Data.Time.LocalTime
import Display.Labels
import Test.Hspec as HS
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: HS.Spec
spec = describe "Labels" $ do
  describe "labels" $ do
    it "generates labels for clean ints" $
      let span = (0, 100) :: (Int, Int)
          input = [0 .. 10] :: [Int]
          expected =
            [ (0, "0"),
              (10, "1"),
              (20, "2"),
              (30, "3"),
              (40, "4"),
              (50, "5"),
              (60, "6"),
              (70, "7"),
              (80, "8"),
              (90, "9"),
              (100, "10")
            ]
       in generateLabelsDiscrete input span `shouldBe` expected
    it "generates labels for clean ints with offset" $
      let span = (0, 100) :: (Int, Int)
          input = [5 .. 15] :: [Int]
          expected =
            [ (0, "5"),
              (10, "6"),
              (20, "7"),
              (30, "8"),
              (40, "9"),
              (50, "10"),
              (60, "11"),
              (70, "12"),
              (80, "13"),
              (90, "14"),
              (100, "15")
            ]
       in generateLabelsDiscrete input span `shouldBe` expected
    it "generates labels for non-integers with offset" $
      let span = (0, 100) :: (Int, Int)
          input = [0.5 .. 1.5] :: [Double]
          expected =
            [ (0, "0.5000"),
              (10, "0.6000"),
              (20, "0.7000"),
              (30, "0.8000"),
              (40, "0.9000"),
              (50, "1.0000"),
              (60, "1.1000"),
              (70, "1.2000"),
              (80, "1.3000"),
              (90, "1.4000"),
              (100, "1.5000")
            ]
       in generateLabelsContinuous input span `shouldBe` expected
    it "generates lables for non-aligning values" $
      let span = (0, 127) :: (Int, Int)
          input = [0 .. 15] :: [Int]
          expected =
            [ (0, "0"),
              (11, "1"),
              (22, "2"),
              (33, "3"),
              (44, "4"),
              (55, "5"),
              (66, "6"),
              (77, "7"),
              (88, "8"),
              (99, "9"),
              (110, "10"),
              (121, "11")
            ]
       in generateLabelsDiscrete input span `shouldBe` expected
    it "generates labels for non-integers" $
      let span = (0, 100) :: (Int, Int)
          input = [0 .. 1] :: [Decimal]
          expected =
            [ (0, "0.0000"),
              (10, "0.1000"),
              (20, "0.2000"),
              (30, "0.3000"),
              (40, "0.4000"),
              (50, "0.5000"),
              (60, "0.6000"),
              (70, "0.7000"),
              (80, "0.8000"),
              (90, "0.9000"),
              (100, "1.0000")
            ]
       in generateLabelsContinuous input span `shouldBe` expected
    it "generates labels for times by aligning to a time unit (ie, days, minutes, etc)"
      . idempotentIOProperty
      $ let ts = 5 `daysFrom` 1555328587
         in generateLabelsTime utc ts (0, 100)
              `shouldBe` [(10, "16/04"), (30, "17/04"), (50, "18/04"), (70, "19/04"), (90, "20/04")]
  describe "step sizes" $ do
    prop "step size is deterministic" $ \step -> (asTime step - asTime step) === 0
    prop "time steps can be composed of smaller steps" $ \step step' ->
      let stepTime = asTime step
          stepTime' = asTime step'
       in max stepTime stepTime' `mod'` min stepTime stepTime' === 0
  describe "determineStepSize" $ do
    prop "when there are less than 5 minutes, 1 minute increments are used"
      . forAll (arbitrary `suchThat` (< Positive 5))
      $ \(Positive minutes) (Positive start) ->
        let end = start + (minutes * 60) in (determineStepSize (start, end) === Minute)
    prop "when there are more than 5 minutes, 5 minute increments are used"
      . forAll (arbitrary `suchThat` (< Positive 50))
      $ \(Positive minutes) (Positive start) ->
        let end = start + 300 + (minutes * 60) in (determineStepSize (start, end) === FiveMinute)
    prop "when there are more minutes than two hours, hour long increments are used"
      . forAll (choose (2, 23))
      $ \hours start -> let end = start + (hours * 3600) in (determineStepSize (start, end) === Hour)
    prop "when there are more minutes than two days, day long increments are used"
      . forAll (choose (2, 7))
      $ \days' start -> let end = start + (days' * 86400) in (determineStepSize (start, end) === Day)
    prop "when there are fewer seconds than a minute then seconds are used"
      . forAll (arbitrary `suchThat` (< Positive 60))
      $ \(Positive seconds) (Positive start) ->
        let end = start + seconds in (determineStepSize (start, end) === Second)
    prop "when there are fewer than one second it becomes milliseconds"
      . forAll (choose (0, 1))
      $ \milliseconds start ->
        let end = start + (milliseconds * 0.001) in determineStepSize (start, end) === Millisecond
