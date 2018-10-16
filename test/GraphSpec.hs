module GraphSpec where

import           Graph
import qualified Data.Aeson                    as Aeson
import           Test.Hspec                    as HS
import           Data.Scientific
import           ArbitraryInstances

spec :: HS.Spec
spec =
  desribe "transforming to display data"
    . it "correctly transforms data to displayable data" $ pending
    -- $ forAll (arbitrary :: Gen [DataPoint])
    -- $ \dps -> (toDisplayData dps) `shouldBe` ???
