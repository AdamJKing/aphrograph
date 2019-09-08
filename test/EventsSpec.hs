{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module EventsSpec
    ( spec
    )
where

import           Test.Hspec                    as HS
import           Test.Hspec.QuickCheck
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
    prop "ignores misc. events that the application doesn't use"
        . monadic' @Property @(MockApp ActiveState)
        $ do
              event                       <- pick (oneof [mouseDown, mouseUp])
              (Continue (Active outcome)) <- run (appEventHandler event)
              testState                   <- run ask
              return $ testState === outcome

    prop "updates the app state from graphite when requested (UpdateEvent)"
        . monadic' @Property @(MockApp (EmptyState ActiveState))
        $ do
              (Empty    testState       ) <- run ask
              (Continue (Active outcome)) <- run
                  (appEventHandler (Brick.AppEvent UpdateEvent))
              return $ testState === outcome

    prop "ends the event loop when an Exit Key is pressed"
        . monadic' @Property @(MockApp ActiveState)
        $ do
              outcome <- run (appEventHandler (Brick.VtyEvent ExitKey))
              return . property $ case outcome of
                  Stop -> True
                  _    -> False
