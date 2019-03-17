module ArgsSpec where

import           Test.Hspec                    as HS
import           App.Args                      as App
import           Test.QuickCheck
import           Time.Types

spec :: HS.Spec
spec = describe "Args" $ describe "parseTime" $ do
    it "parses seconds" $ readTime "30s" === Right (Seconds 30)
    it "parses minutes" $ readTime "60m" === Right (Seconds 3600)
    it "parses seconds" $ readTime "24h" === Right (Seconds 86400)
    it "parses seconds" $ readTime "7d" === Right (Seconds 604800)

    it "returns nothing on errors"
        $ let outcome = readTime "gibberish"
          in  outcome `shouldBe` Left "Prelude.read: no parse"


    it "returns nothing for an invalid time unit"
        $ let outcome = readTime "5l"
          in  outcome `shouldBe` Left "Invalid time. (5l)"

    it "returns nothing for a missing time unit"
        $ let outcome = readTime "5"
          in  outcome `shouldBe` Left "Invalid time. (5)"
