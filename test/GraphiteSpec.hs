{-# LANGUAGE OverloadedStrings #-}
module GraphiteSpec where

import           Graphite
import qualified Data.Aeson                    as Aeson
import           Test.Hspec                    as HS
import           Data.Scientific

spec :: HS.Spec
spec =
  describe "Parsing a decimal"
    .          it "should correctly parse a valid decimal"
    $          Aeson.eitherDecode "0.55655" `shouldBe` Right (0.55655 :: Scientific)
