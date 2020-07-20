{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Display.WidgetsSpec where

import ArbitraryInstances ()
import Test.Hspec as HS
import Relude

spec :: HS.Spec
spec = describe "Widgets" $ it "has no purpose yet" $ True `shouldBe` True
