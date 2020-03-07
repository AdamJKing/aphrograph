module DisplaySpec where

import           ArbitraryInstances             ( )
import           Test.Hspec                    as HS
import           Prelude                 hiding ( null )
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           App.Components
import           App                            ( constructDom )
import qualified App.State                     as App

spec :: HS.Spec
spec = describe "Display" $ describe "constructDom" $ prop "choose the right widget for the app state" $ \appState ->
  case constructDom (App.Active appState) of
    DisplayWidget (Right (DefaultDisplay _ _)) -> property True
    _ -> property False
