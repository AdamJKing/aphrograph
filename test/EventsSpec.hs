{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module EventsSpec where

import           Test.Hspec                    as HS
import           Brick.Types                   as Brick
import           Test.QuickCheck
import           ArbitraryInstances             ( )
import           Control.Monad.Log
import           Events
import           App.Args                      as App
import           Data.Hourglass
import           Graphite

data DummyComponent = DummyComponent deriving (Eq, Show)

instance MonadGraphite (TestM Gen) where
    getMetricsForPast _ _ = lift arbitrary

newtype TestM m a = TestM (ReaderT App.Args (DiscardLoggingT Text m) a)
    deriving (Functor, Applicative, Monad, MonadLog Text, MonadReader App.Args)

instance MonadTrans TestM where
    lift m = TestM (lift (lift m))


runTestM :: App.Args -> TestM Gen a -> Gen a
runTestM args (TestM stack) = discardLogging $ usingReaderT args stack

vtyEvent :: Gen (BrickEvent n e)
vtyEvent = Brick.VtyEvent <$> arbitrary

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
    it "ignores misc. events that the application doesn't use"
        . forAll (oneof [vtyEvent, mouseDown, mouseUp])
        $ \event ->
              let args = App.Args (Seconds 100) "test.target"
              in  runTestM args $ do
                      initialState <- lift arbitrary
                      outcome      <- appEventHandler event initialState
                      return (outcome === Continue initialState)

    it "updates the app state from graphite when requested (UpdateEvent)"
        . property
        $ let args = App.Args (Seconds 100) "test.target"
          in  runTestM args $ do
                  initialState <- lift arbitrary
                  outcome      <- appEventHandler (Brick.AppEvent UpdateEvent)
                                                  initialState
                  return (outcome =/= Continue initialState)

    it "ends the event loop when an Exit Key is pressed"
        . property
        $ let args = App.Args (Seconds 100) "test.target"
          in  runTestM args $ do
                  initialState <- lift arbitrary
                  outcome      <- appEventHandler (Brick.VtyEvent ExitKey)
                                                  initialState
                  return (outcome === Stop)