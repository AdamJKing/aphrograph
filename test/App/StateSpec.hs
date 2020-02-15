{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.StateSpec
  ( spec,
  )
where

import App.State as App
import ArbitraryInstances ()
import CommonProperties
import Control.Lens.Operators
import Control.Lens.Prism
import Test.Hspec
  ( Spec,
    describe,
  )
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Monadic

spec :: Spec
spec = describe "App.State" $ describe "Graph updates" $ prop "overwriting existing graph state" $ runMonadicTest $ do
  newGraph <- pick arbitrary
  oldState <- pick activeState
  result <- run (updateGraph (return newGraph) oldState)
  assert (result ^? (App.appData . _Right . App.graphData) == Just newGraph)
