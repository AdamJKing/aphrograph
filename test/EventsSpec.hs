{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module EventsSpec
    ( spec
    )
where

import           Test.Hspec                    as HS
import           Brick.Types                   as Brick
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           ArbitraryInstances             ( )
import           Events
import           App
import           CommonProperties

data DummyComponent = DummyComponent deriving (Eq, Show)

mouseDown :: Gen (BrickEvent DummyComponent e)
mouseDown = do
    loc    <- arbitrary
    button <- arbitrary
    return (MouseDown DummyComponent button [] (Location loc))

mouseUp :: Gen (BrickEvent DummyComponent e)
mouseUp = do
    loc    <- arbitrary
    button <- arbitrary
    return (MouseUp DummyComponent button (Location loc))

spec :: HS.Spec
spec = describe "Events" $ do
    appProp "ignores misc. events that the application doesn't use" $ do
        event     <- pick (oneof [mouseDown, mouseUp])
        outcome   <- run (appEventHandler event)
        testState <- run ask
        return (outcome `shouldBe` Continue (Active testState))

    appProp "updates the app state from graphite when requested (UpdateEvent)"
        $ do
              testState <- run ask
              outcome   <- run (appEventHandler (Brick.AppEvent UpdateEvent))
              return (outcome `shouldNotBe` Update (Active testState))

    appProp "ends the event loop when an Exit Key is pressed" $ do
        outcome <- run (appEventHandler (Brick.VtyEvent ExitKey))
        return (outcome === Stop)
