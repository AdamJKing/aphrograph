module DisplaySpec where

import App (constructDom)
import App.Components
import qualified App.State as App
import ArbitraryInstances ()
import Test.Hspec as HS
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Prelude hiding (null)

spec :: HS.Spec
spec = describe "Display" $ describe "constructDom" $ prop "choose the right widget for the app state" $ \appState ->
  case constructDom (App.Active appState) of
    DisplayWidget (Right (DefaultDisplay _ _)) -> property True
    _ -> property False
