module DisplaySpec where

import ArbitraryInstances ()
import Test.Hspec as HS
import Test.Hspec.QuickCheck
import Prelude hiding (null)

spec :: HS.Spec
spec = describe "Display" $ describe "constructDom" $ prop "choose the right widget for the app state" $ pending
