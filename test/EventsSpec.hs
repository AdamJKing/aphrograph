{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module EventsSpec
  ( spec,
  )
where

import ArbitraryInstances ()
import Test.Hspec as HS
import Test.Hspec.QuickCheck (prop)
import Test.Orphans ()

data DummyComponent = DummyComponent deriving (Eq, Show)

-- mouseDown :: Gen (BrickEvent DummyComponent ())
-- mouseDown = do
--   loc <- arbitrary
--   button <- arbitrary
--   return (MouseDown DummyComponent button [] (Location loc))

-- mouseUp :: Gen (BrickEvent DummyComponent ())
-- mouseUp = do
--   loc <- arbitrary
--   button <- arbitrary
--   return (MouseUp DummyComponent button (Location loc))

spec :: HS.Spec
spec = describe "Events" $ do
  prop "ignores misc. Brick events that the application doesn't use" $ pendingWith "we have no way to know if the application did or did not respond to an event"
  prop "ends the event loop when an Exit Key is pressed" $ pendingWith "uses test implementation and doesn't test real code"
