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
import           ArbitraryInstances             ( )
import           Events
import           App

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
  prop "ignores misc. events that the application doesn't use" $ do
    event      <- oneof [mouseDown, mouseUp]
    startState <- arbitrary
    let (Continue (Active outcome)) = appEventHandler (SystemEvent event) startState
    return $ startState === outcome

  prop "updates the app state from graphite when requested (UpdateEvent)" pending
      -- . monadic'
      -- $ do
      --       testState                   <- run ask
      --       (Continue (Active outcome)) <- run
      --           (appEventHandler (Brick.AppEvent UpdateEvent))
      --       return 

  prop "ends the event loop when an Exit Key is pressed" $ \startState ->
    let exitEvent = SystemEvent (Brick.VtyEvent ExitKey)
        outcome   = appEventHandler exitEvent startState
    in  case outcome of
          (Stop _) -> True
          _        -> False
