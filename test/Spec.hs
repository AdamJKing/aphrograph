module Main where

import qualified Display.GraphSpec (spec)
import qualified Display.ProjectionSpec (spec)
import qualified Display.WidgetsSpec (spec)
import qualified DisplaySpec (spec)
import qualified GraphiteSpec (spec)
import qualified LabelsSpec (spec)
import qualified NormalisationSpec (spec)
import Test.Hspec
import Relude
import qualified WidgetsSpec (spec)

main :: IO ()
main = hspec $ do
  describe "Display" DisplaySpec.spec
  describe "Graph" Display.GraphSpec.spec
  describe "Projection" Display.ProjectionSpec.spec
  describe "Widgets" Display.WidgetsSpec.spec
  describe "Graphite" GraphiteSpec.spec
  describe "Labels" LabelsSpec.spec
  describe "Normalisation" NormalisationSpec.spec
  describe "Widgets" WidgetsSpec.spec
