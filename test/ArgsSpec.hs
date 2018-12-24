module ArgsSpec where

import           Test.Hspec                    as HS
import           Args
import           Test.QuickCheck
import           Time.Types

spec :: HS.Spec
spec = describe "Args" $ do
    describe "parseTime" $ do
        it "parses seconds" $ parseArg "30s" === Right (Seconds 30)
        it "parses minutes" $ parseArg "60m" === Right (Seconds 3600)
        it "parses seconds" $ parseArg "24h" === Right (Seconds 86400)
        it "parses seconds" $ parseArg "7d" === Right (Seconds 604800)

        it "returns nothing on errors"
            $ let outcome :: Either Text Seconds
                  outcome = parseArg "gibberish"
              in  outcome `shouldBe` Left "Invalid time. (gibberish)"


        it "returns nothing for an invalid time unit"
            $ let outcome :: Either Text Seconds
                  outcome = parseArg "5l"
              in  outcome `shouldBe` Left "Invalid time. (5l)"

        it "returns nothing for a missing time unit"
            $ let outcome :: Either Text Seconds
                  outcome = parseArg "5"
              in  outcome `shouldBe` Left "Invalid time. (5)"

    describe "getAppArgs" $ do
        it "parses the app arguments"
            $          parseAppArgs ["example.target", "30s"]
            `shouldBe` (Right $ AppArgs
                           { _target = "example.target"
                           , _time   = Seconds 30
                           }
                       )

        it "fails to parse no arguments" $ parseAppArgs [] `shouldBe` Left
            "aphrograph-exe $TARGET $TIME"

        it "fails to parse missing arguments"
            $          parseAppArgs ["example.target"]
            `shouldBe` Left "aphrograph-exe $TARGET $TIME"

