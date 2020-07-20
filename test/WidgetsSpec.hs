module WidgetsSpec
  ( spec,
  )
where

import ArbitraryInstances ()
import Data.Time.LocalTime
import Display.GraphWidget
import qualified Graphics.Vty.Image as Vty
import Relude
import Test.Hspec as HS
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: HS.Spec
spec = describe "normalise" $ do
  prop "vertical axis widget is never empty" $ \(Positive height) values -> drawVerticalAxisImage height values =/= Vty.emptyImage
  prop "horizontal axis widget is never empty" $ \(Positive width) values -> drawHorizontalAxisImage utc width values =/= Vty.emptyImage
