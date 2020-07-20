{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.StateSpec
  ( spec,
  )
where

import ArbitraryInstances ()
import Relude
import Test.Hspec
import Test.Hspec.QuickCheck (prop)

spec :: Spec
spec =
  describe "App.State" $
    describe "Graph updates" $
      prop "overwriting existing graph state" pending
