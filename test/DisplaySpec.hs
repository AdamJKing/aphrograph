module DisplaySpec where

import           ArbitraryInstances             ( )
import           Test.Hspec                    as HS
import           Prelude                 hiding ( null )
import           Display
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

spec :: HS.Spec
spec = describe "Display" $ describe "constructDom" $ prop "choose the right widget for the app state" $ \appState ->
  case constructDom (Right appState) of
    DisplayWidget (Right (DefaultDisplay _ _)) -> property True
    _ -> property False
