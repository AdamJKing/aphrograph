{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module EventsSpec
    ( spec
    )
where

import           Test.Hspec                    as HS
import           Brick.Types                   as Brick
import           Test.QuickCheck
import           Test.Hspec.QuickCheck
import           ArbitraryInstances             ( )
import           Control.Monad.Log
import           Events
import           App.Args                      as App
import           Graphite


data DummyComponent = DummyComponent deriving (Eq, Show)

instance MonadGraphite TestIO where
    getMetricsForPast _ _ _ = arbitraryTestIO

newtype TestIO a = TestIO (ReaderT App.Args (DiscardLoggingT Text IO) a)
    deriving (Functor, Applicative, Monad, MonadLog Text, MonadReader App.Args, MonadIO)

arbitraryTestIO :: (Arbitrary a) => TestIO a
arbitraryTestIO = liftIO $ generate arbitrary

instance (Testable t) => Testable ( TestIO t ) where
    property (TestIO t) = idempotentIOProperty $ do
        args <- generate (applyArbitrary4 App.Args)
        discardLogging $ usingReaderT args t

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
        . forAll (oneof [mouseDown, mouseUp])
        $ \event -> do
              initialState <- arbitraryTestIO
              outcome      <- appEventHandler event initialState
              return (outcome === Continue initialState)

    prop "updates the app state from graphite when requested (UpdateEvent)" $ do
        initialState <- arbitraryTestIO
        outcome <- appEventHandler (Brick.AppEvent UpdateEvent) initialState
        return (outcome =/= Continue initialState)

    prop "ends the event loop when an Exit Key is pressed" $ do
        initialState <- arbitraryTestIO
        outcome      <- appEventHandler (Brick.VtyEvent ExitKey) initialState
        return (outcome === Stop)
