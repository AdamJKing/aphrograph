import           Graphite
import           Test.Hspec

main :: IO ()
main =
  hspec $ do
    describe "Graphite.metricRespToText" $ do
      it "converts a metric response to a single value when present" $
        metricRespToText exampleMetricResponse `shouldBe` "1.0"
          where
            exampleMetricResponse = MetricResponse {target = "the.target", datapoints = [exampleDataPoint]}
            exampleDataPoint = Graphite.DataPoint {value = Just 1.0, time = 123456}

      it "converts a metric response with a null value to \"no value\"" $
        metricRespToText exampleMetricResponse `shouldBe` "No value"
          where
            exampleMetricResponse = MetricResponse {target = "the.target", datapoints = [exampleDataPoint]}
            exampleDataPoint = Graphite.DataPoint {value = Just 1.0, time = 123456}
