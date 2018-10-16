{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import           Data.Scientific
import qualified GraphiteSpec                   ( spec )
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
import           Normalisation

import           Graphite

main :: IO ()
main = hspec $ do
  describe "Graphite" GraphiteSpec.spec
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
          normaliseFractional (a, b) (a, b) i === i
    it "produces a value x times larger when the second range is x times larger"
      . forAll (genTestData :: Gen (Range Double, Positive Double))
      $ \((Positive a, Positive b), Positive v) -> do
          Positive f <- resize 500 arbitrary :: Gen (Positive Double)
          return $ floor (normaliseFractional (a, b) (a * f, b * f) v) === floor
            (v * f)
  describe "Main.parseTime" $ do
    it "parses seconds" $ parseTime "30s" === Just (Seconds 30)
    it "parses minutes" $ parseTime "60m" === Just (Seconds 3600)
    it "parses seconds" $ parseTime "24h" === Just (Seconds 86400)
    it "parses seconds" $ parseTime "7d" === Just (Seconds 604800)
    it "returns left on errors" $ parseTime "gibberish" === Nothing
