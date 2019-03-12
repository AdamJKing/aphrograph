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
            [ Label "0"  0
            , Label "1"  10
            , Label "2"  20
            , Label "3"  30
            , Label "4"  40
            , Label "5"  50
            , Label "6"  60
            , Label "7"  70
            , Label "8"  80
            , Label "9"  90
            , Label "10" 100
            ]
      in  (generateLabelsDiscrete input span `shouldBe` expected)

  it "generates lables for non-aligning values"
    $ let span  = (0, 127) :: (Int, Int)
          input = [0 .. 15]
          expected =
            [ Label "0"  0
            , Label "1"  11
            , Label "2"  22
            , Label "3"  33
            , Label "4"  44
            , Label "5"  55
            , Label "6"  66
            , Label "7"  77
            , Label "8"  88
            , Label "9"  99
            , Label "10" 110
            , Label "11" 121
            ]
      in  (generateLabelsDiscrete input span `shouldBe` expected)

  it "generates labels for non-integers"
    $ let span  = (0, 100) :: (Int, Int)
          input = [0 .. 1] :: [Decimal]
          expected =
            [ Label "0" 0
            , Label "0.1" 10
            , Label "0.2" 20
            , Label "0.3" 30
            , Label "0.4" 40
            , Label "0.5" 50
            , Label "0.6" 60
            , Label "0.7" 70
            , Label "0.8" 80
            , Label "0.9" 90
            , Label "1" 100
            ]
      in  (generateLabelsContinuous input span `shouldBe` expected)
