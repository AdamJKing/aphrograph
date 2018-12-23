{-# LANGUAGE ScopedTypeVariables #-}
module LabelsSpec where

import           Test.Hspec                    as HS
import           Data.Decimal
import           Labels


spec :: HS.Spec
spec = describe "Labels" $ describe "organiseLabels" $ do
        it
                        "creates periodic labels that fit into the max width (discrete, non reducing)"
                $ let range :: (DiscreteValue Int, DiscreteValue Int)
                      range         = (Discrete 0, Discrete 10)
                      expectedlabel = "  0  1  2  3  4  5  6  7  8  9 10"
                  in  organiseLabels (length expectedlabel) range
                              `shouldBe` expectedlabel

        it
                        "creates periodic labels that fit into the max width (continuous, non reducing)"
                $ let
                          range :: (Decimal, Decimal)
                          range = (0, 1.0)
                          expectedlabel
                                  = "  0.0  0.1  0.2  0.3  0.4  0.5  0.6  0.7  0.8  0.9  1.0"
                  in
                          organiseLabels (length expectedlabel) range
                                  `shouldBe` expectedlabel

        it
                        "creates periodic labels that fit into the max width (discrete, reducing)"
                $ let range :: (DiscreteValue Int, DiscreteValue Int)
                      range         = (Discrete 0, Discrete 20)
                      expectedlabel = "  0  2  4  6  8 10 12 14 16 18 20"
                  in  organiseLabels (length expectedlabel) range
                              `shouldBe` expectedlabel

        it
                        "creates periodic labels that fit into the max width (continuous, reducing)"
                $ let
                          range :: (Decimal, Decimal)
                          range = (0, 2.0)
                          expectedlabel
                                  = "  0.0  0.2  0.4  0.6  0.8  1.0  1.2  1.4  1.6  1.8  2.0"
                  in
                          organiseLabels (length expectedlabel) range
                                  `shouldBe` expectedlabel

        it
                        "creates periodic labels that fit into the max width (discrete, with minimal)"
                $ let range :: (DiscreteValue Int, DiscreteValue Int)
                      range         = (Discrete 7, Discrete 15)
                      expectedlabel = "  7  8  9 10 11 12 13 14 15"
                  in  organiseLabels (length expectedlabel) range
                              `shouldBe` expectedlabel

        it
                        "creates periodic labels that fit into the max width (continuous, with minimal)"
                $ let
                          range :: (Decimal, Decimal)
                          range = (0.07, 0.15)
                          expectedlabel
                                  = " 0.07 0.08 0.09 0.10 0.11 0.12 0.13 0.14 0.15"
                  in
                          organiseLabels (length expectedlabel) range
                                  `shouldBe` expectedlabel
