
module DisplaySpec where

import           ArbitraryInstances             ( )
import           Test.Hspec                    as HS
import           Prelude                 hiding ( null )
import           App
import           Display
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

spec :: HS.Spec
spec = describe "Display" $ describe "constructDom" $ prop "choose the right widget for the app state" $ \appState ->
  case (appState, constructDom appState) of
    (Active _, DefaultDisplay _ _) -> property True
    (Failed _, ErrorDisplay _    ) -> property True
    _                              -> counterexample "The state was not mapped to the correct display" False
