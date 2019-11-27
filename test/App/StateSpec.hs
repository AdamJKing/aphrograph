
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

spec :: Spec
spec =
    describe "App.State"
        $ describe "Graph updates"
        $ prop "errors updating the graph results in failed state"
        $ runMonadicTest
        $ do
              startState <- pick arbitrary
              err        <- pick arbitrary
              run (assign getMetricsResponse (Left err))
              result <- run (updateGraph startState)

              assert (is App.failed result)
