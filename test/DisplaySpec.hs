module DisplaySpec where

import App (constructDom)
import App.Components
import ArbitraryInstances ()
import CommonProperties
import Test.Hspec as HS
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Prelude

spec :: HS.Spec
spec = describe "Display" $ describe "constructDom" $ prop "choose the right widget for the app state" $ forAll activeState $ \appState ->
  case constructDom appState of
    DisplayWidget (Right (DefaultDisplay _ _)) -> property True
    _ -> property False
