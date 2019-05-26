
module Display.DisplaySpec where

import           Test.Hspec                    as HS
import           Test.QuickCheck
import           Test.Hspec.QuickCheck
import           Display.Types


spec :: HS.Spec
spec =
    describe "Display"
        $ prop "dimensions from a tuple"
        . forAll (arbitrary2 :: Gen (Int, Int))
        $ \(w, h) -> dim (w, h) === Dimensions w h

