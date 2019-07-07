
module DisplaySpec where

import           ArbitraryInstances             ( )
import           Test.Hspec                    as HS
import           Prelude                 hiding ( null )
import           App
import           Display
import           Test.Hspec.QuickCheck

spec :: HS.Spec
spec =
    describe "Display"
        $ describe "constructDom"
        $ prop "choose the right widget for the app state"
        $ \appState ->
              let (AppWidget result) = constructDom appState
              in  if hasFailed appState then isLeft result else isRight result
