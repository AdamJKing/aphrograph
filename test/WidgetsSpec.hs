module WidgetsSpec
  ( spec,
  )
where

import ArbitraryInstances ()
import Data.Time (utc)
import Display.GraphWidget
  ( drawHorizontalAxisImage,
    drawVerticalAxisImage,
  )
import qualified Graphics.Vty.Image as Vty
import Test.Hspec as HS (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Positive (Positive), (=/=))

spec :: HS.Spec
spec = describe "normalise" $ do
  prop "vertical axis widget is never empty" $
    \(Positive height) values ->
      drawVerticalAxisImage height values =/= Vty.emptyImage

  prop "horizontal axis widget is never empty" $
    \(Positive width) values ->
      drawHorizontalAxisImage utc width values =/= Vty.emptyImage
