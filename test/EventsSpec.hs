{-# LANGUAGE MultiParamTypeClasses #-}
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
import           Events                        as E

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
  prop "ignores misc. brick events that the application doesn't use" $ forAll (oneof [mouseDown, mouseUp]) $ \e ->
    let handler = EventHandler { continue = const (return $ Just False)
                               , ignore   = const (return $ Just True)
                               , stop     = const (return Nothing)
                               }
    in  monadic runIdentity $ run (handleEvent handler e False)

  it "updates the app state from graphite when requested (UpdateEvent)" pending

  it "ends the event loop when an Exit Key is pressed"                  pending
    -- $          handleBrickEvents (Brick.VtyEvent E.ExitKey) 
    -- `shouldBe` Just ExitEvent
