
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PartialTypeSignatures #-}

module App.StateSpec
    ( spec
    )
where

import           Test.Hspec                     ( Spec
                                                , describe
                                                )
import           Test.Hspec.QuickCheck          ( prop )
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           ArbitraryInstances             ( )
import           App.State                     as App
import           CommonProperties
import           Control.Lens.Extras            ( is )
import           Control.Lens.Setter
import           Control.Lens.Operators

spec :: Spec
spec = describe "App.State" $ describe "Graph updates" $ prop "overwriting existing graph state" $ runMonadicTest $ do
    newGraph <- pick arbitrary
    oldState <- pick activeState
    result   <- run (updateGraph (return newGraph) oldState)
    assert (is App.active result)
    assert (result ^? (App.active . App.graphData) == Just newGraph)
