{-# LANGUAGE FlexibleInstances #-}

module Main where

import           Test.Hspec.Runner
import           Test.Hspec
import qualified GraphSpec                      ( spec )
import qualified GraphiteSpec                   ( spec )
import qualified LabelsSpec                     ( spec )
import qualified ArgsSpec                       ( spec )
import qualified NormalisationSpec              ( spec )
import qualified EventsSpec              ( spec )

main :: IO ()
main = hspec $ do
  describe "Graph"         GraphSpec.spec
  describe "Events"        EventsSpec.spec
  describe "Graphite"      GraphiteSpec.spec
  describe "Args"          ArgsSpec.spec
  describe "Labels"        LabelsSpec.spec
  describe "Normalisation" NormalisationSpec.spec
