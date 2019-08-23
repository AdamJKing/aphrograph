{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ApplicativeDo #-}

module GraphiteSpec where

import           Test.Hspec                    as HS
import           ArbitraryInstances             ( )
import           Graphite.Types
import qualified Data.Aeson                    as JSON
import           Data.Aeson                     ( (.=) )
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
      $ let json = JSON.Array $ fromList
              [ JSON.object
                  [ "datapoints"
                    .= (JSON.Array $ fromList
                         (   JSON.Array
                         .   fromList
                         <$> [ [JSON.Number 0, JSON.Number 1000]
                             , [JSON.Number 1, JSON.Number 2000]
                             , [JSON.Number 2, JSON.Number 3000]
                             ]
                         )
                       )
                  , "target" .= JSON.String "test"
                  , "tags" .= JSON.object ["name" .= JSON.String "test"]
                  ]
              ]
        in  case JSON.fromJSON json of
              JSON.Success [MetricsResponse {..}] -> do
                target `shouldBe` "test"
                tags `shouldBe` one ("name", "test")
                datapoints
                  `shouldMatchList` [ DataPoint 0 1000
                                    , DataPoint 1 2000
                                    , DataPoint 2 3000
                                    ]
              JSON.Success other ->
                expectationFailure
                  $  "Parser returned an unexpected value: "
                  ++ show other
              JSON.Error err -> expectationFailure err

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
