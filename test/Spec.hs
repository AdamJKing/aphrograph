{-# LANGUAGE FlexibleInstances #-}

module Main where

import           Test.Hspec.Runner
import           Test.Hspec
import qualified Display.DisplaySpec            ( spec )
import qualified Display.GraphSpec              ( spec )
import qualified Display.GraphBuilderSpec       ( spec )
import qualified Display.ProjectionSpec         ( spec )
import qualified Display.WidgetsSpec            ( spec )
import qualified GraphiteSpec                   ( spec )
import qualified LabelsSpec                     ( spec )
import qualified NormalisationSpec              ( spec )
import qualified EventsSpec                     ( spec )

main :: IO ()
main = hspec $ do
  describe "Display"       Display.DisplaySpec.spec
  describe "GraphBuilder"  Display.GraphBuilderSpec.spec
  describe "Graph"         Display.GraphSpec.spec
  describe "Projection"    Display.ProjectionSpec.spec
  describe "Widgets"       Display.WidgetsSpec.spec
  describe "Events"        EventsSpec.spec
  describe "Graphite"      GraphiteSpec.spec
  describe "Labels"        LabelsSpec.spec
  describe "Normalisation" NormalisationSpec.spec
