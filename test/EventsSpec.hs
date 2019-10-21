{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module EventsSpec
  ( spec
  )
where

import           Test.Hspec                    as HS
import           Test.Hspec.QuickCheck
-- import           Brick.Types                   as Brick
-- import           Test.QuickCheck
import           ArbitraryInstances             ( )
-- import           Events                        as E

data DummyComponent = DummyComponent deriving (Eq, Show)

-- mouseDown :: Gen (BrickEvent DummyComponent e)
-- mouseDown = do
--   loc    <- arbitrary
--   button <- arbitrary
--   return (MouseDown DummyComponent button [] (Location loc))

-- mouseUp :: Gen (BrickEvent DummyComponent e)
-- mouseUp = do
--   loc    <- arbitrary
--   button <- arbitrary
--   return (MouseUp DummyComponent button (Location loc))

spec :: HS.Spec
spec = describe "Events" $ do
  prop "ignores misc. brick events that the application doesn't use" $ pending
    -- $ forAll (oneof [mouseDown, mouseUp])
    -- $ isNothing
    -- . handleBrickEvents

  prop "updates the app state from graphite when requested (UpdateEvent)" pending
      -- . monadic'
      -- $ do
      --       testState                   <- run ask
      --       (Continue (Active outcome)) <- run
      --           (appEventHandler (Brick.AppEvent UpdateEvent))
      --       return 

  it "ends the event loop when an Exit Key is pressed" $ pending
    -- $          handleBrickEvents (Brick.VtyEvent E.ExitKey) 
    -- `shouldBe` Just ExitEvent
