{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module LabelsSpec where

import           Test.Hspec                    as HS
import           Display.Labels
import           Data.Decimal


spec :: HS.Spec
spec = describe "Labels" . describe "labels" $ do
  it "generates labels for clean ints"
    $ let span  = (0, 100) :: (Int, Int)
          input = [0 .. 10]
          expected =
            [ (0  , "0")
            , (10 , "1")
            , (20 , "2")
            , (30 , "3")
            , (40 , "4")
            , (50 , "5")
            , (60 , "6")
            , (70 , "7")
            , (80 , "8")
            , (90 , "9")
            , (100, "10")
            ]
      in  (generateLabelsDiscrete input span `shouldBe` expected)

  it "generates labels for clean ints with offset"
    $ let span  = (0, 100) :: (Int, Int)
          input = [5 .. 15]
          expected =
            [ (0  , "5")
            , (10 , "6")
            , (20 , "7")
            , (30 , "8")
            , (40 , "9")
            , (50 , "10")
            , (60 , "11")
            , (70 , "12")
            , (80 , "13")
            , (90 , "14")
            , (100, "15")
            ]
      in  (generateLabelsDiscrete input span `shouldBe` expected)
      
  it "generates labels for non-integers with offset"
    $ let span  = (0, 100) :: (Int, Int)
          input = [0.5 .. 1.5] :: [Double]
          expected =
            [ (0  , "0.5000")
            , (10 , "0.6000")
            , (20 , "0.7000")
            , (30 , "0.8000")
            , (40 , "0.9000")
            , (50 , "1.0000")
            , (60 , "1.1000")
            , (70 , "1.2000")
            , (80 , "1.3000")
            , (90 , "1.4000")
            , (100, "1.5000")
            ]
      in  (generateLabelsContinuous input span `shouldBe` expected)

  it "generates lables for non-aligning values"
    $ let span  = (0, 127) :: (Int, Int)
          input = [0 .. 15]
          expected =
            [ (0  , "0")
            , (11 , "1")
            , (22 , "2")
            , (33 , "3")
            , (44 , "4")
            , (55 , "5")
            , (66 , "6")
            , (77 , "7")
            , (88 , "8")
            , (99 , "9")
            , (110, "10")
            , (121, "11")
            ]
      in  (generateLabelsDiscrete input span `shouldBe` expected)

  it "generates labels for non-integers"
    $ let span  = (0, 100) :: (Int, Int)
          input = [0 .. 1] :: [Decimal]
          expected =
            [ (0  , "0.0000")
            , (10 , "0.1000")
            , (20 , "0.2000")
            , (30 , "0.3000")
            , (40 , "0.4000")
            , (50 , "0.5000")
            , (60 , "0.6000")
            , (70 , "0.7000")
            , (80 , "0.8000")
            , (90 , "0.9000")
            , (100, "1.0000")
            ]
      in  (generateLabelsContinuous input span `shouldBe` expected)
