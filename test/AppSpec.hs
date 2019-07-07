module AppSpec where

import           Test.Hspec                    as HS
import           Prelude                 hiding ( null )
import           App
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           ArbitraryInstances             ( )

spec :: HS.Spec
spec =
    describe "App"
        $ describe "hasFailed"
        $ prop "identifies failed states"
        $ \appState -> hasFailed appState === case appState of
              (AppState       _) -> False
              (FailedAppState _) -> True

