{-# LANGUAGE FlexibleInstances #-}

module Main where

import qualified AppSpec (spec)
import qualified Display.GraphSpec (spec)
import qualified Display.ProjectionSpec (spec)
import qualified Display.WidgetsSpec (spec)
import qualified DisplaySpec (spec)
import qualified EventsSpec (spec)
import qualified GraphiteSpec (spec)
import qualified LabelsSpec (spec)
import qualified NormalisationSpec (spec)
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "App" AppSpec.spec
  -- describe "App.State"     StateSpec.spec
  describe "Display" DisplaySpec.spec
  describe "Graph" Display.GraphSpec.spec
  describe "Projection" Display.ProjectionSpec.spec
  describe "Widgets" Display.WidgetsSpec.spec
  describe "Events" EventsSpec.spec
  describe "Graphite" GraphiteSpec.spec
  describe "Labels" LabelsSpec.spec
  describe "Normalisation" NormalisationSpec.spec
