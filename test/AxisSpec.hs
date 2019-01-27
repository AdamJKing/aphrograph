
module AxisSpec where

import           Test.Hspec                    as HS
import           Data.Decimal
import           Labels


spec :: HS.Spec
spec = describe "Axis" $ do
    describe "creating a vertical axis"
        . it "creates axis of vertical data"
        $ let img = (mkVerticalAxis [1, 2, 3, 4, 5]) in (show img) `shouldBe`
        (filter ' '
         "1
          2
          3
          4
          5")

    describe "Horizontal"
