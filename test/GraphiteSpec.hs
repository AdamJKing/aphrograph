{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ApplicativeDo #-}
module GraphiteSpec where

import           Test.Hspec                    as HS
import           ArbitraryInstances             ( )
import           Graphite
import           Graphite.Types
import qualified Data.Aeson                    as JSON
import           Test.QuickCheck
import           Test.Hspec.QuickCheck

spec :: HS.Spec
spec = describe "Graphite" $ do
  describe "Time" . describe "Time deltas" $ do
    prop "delta days" $ \(Positive (Small n)) earliest ->
      let latest = earliest + (fromIntegral n * 86400)
      in  (deltaDays earliest latest === fromIntegral n)

    prop "delta hours" $ \(Positive (Small n)) earliest ->
      let latest = earliest + (fromIntegral n * 3600)
      in  (deltaHours earliest latest === fromIntegral n)

    prop "delta minutes" $ \(Positive (Small n)) earliest ->
      let latest = earliest + (fromIntegral n * 60)
      in  (deltaMinutes earliest latest === fromIntegral n)

  describe "JSON Parsing" $ do
    it "decodes metric responses"
      $ let
          outcome =
            parseMetricTimeSeries
              "[{\"datapoints\": [[0, 1000], [1, 2000], [2, 3000]], \"target\": \"test\", \"tags\": { \"name\": \"test\" } }]"
        in  case outcome of
              Right response ->
                response
                  `shouldBe` [ DataPoint 0 1000
                             , DataPoint 1 2000
                             , DataPoint 2 3000
                             ]

              Left err -> expectationFailure $ "Left: " ++ err

    it "decodes times from JSON entities" $ do
      JSON.decode "12345" `shouldBe` Just (12345 :: Time)
      JSON.decode "1.1" `shouldBe` Just (1.1 :: Time)
      JSON.decode "gibberish" `shouldBe` (Nothing :: Maybe Time)

    it "decodes values from JSON entities" $ do
      JSON.decode "1.1" `shouldBe` Just (1.1 :: Value)
      JSON.decode "gibberish" `shouldBe` (Nothing :: Maybe Value)

    it "decodes data-points from JSON entities" $ do
      JSON.decode "[ 0.555, 155005500 ]"
        `shouldBe` Just (DataPoint 0.555 155005500)
      JSON.decode "[ 0.555 ]" `shouldBe` (Nothing :: Maybe DataPoint)
      JSON.decode "[ 0.555 ]" `shouldBe` (Nothing :: Maybe DataPoint)
      JSON.decode "[ ]" `shouldBe` (Nothing :: Maybe DataPoint)

    it "treats null values as zero"
      $          JSON.decode "[ null, 155005500 ]"
      `shouldBe` Just (DataPoint 0.0 155005500)
