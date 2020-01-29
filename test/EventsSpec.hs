{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module EventsSpec
  ( spec
  )
where

import           Test.Hspec                    as HS
import           Test.Hspec.QuickCheck          ( prop )
import           Brick.Types                   as Brick
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           ArbitraryInstances             ( )
import qualified Events                        as E
import qualified App.State                     as App
import           CommonProperties
import           Test.Orphans                   ( )
import qualified Display.Graph                 as Graph
import           Control.Lens.Extras            ( is )
import           Control.Lens.Getter            ( view )
import           Control.Lens.Setter

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
  prop "ignores misc. Brick events that the application doesn't use" $ runMonadicTest $ do
    startState                          <- pick arbitrary
    event                               <- pick $ oneof [mouseDown, mouseUp]
    (outcome :: E.EventOutcome, result) <- run (E.handleBrickEvent event startState)
    (secondOutcome            , _     ) <- run (E.handleBrickEvent event startState)

    assert $ is App.active result == is App.active startState
    assert $ outcome == secondOutcome

  prop "ends the event loop when an Exit Key is pressed" $ runMonadicTest $ do
    startState   <- pick arbitrary
    (outcome, _) <- run (E.handleKeyPress E.ExitKey startState)
    return (outcome === E.Stop)
