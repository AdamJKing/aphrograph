{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import qualified Events                        as E
import qualified App.State                     as App
import qualified App.Config                    as App
import           CommonProperties
import           Test.Orphans                   ( )
import qualified Display.Graph                 as Graph

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
  appProp "ignores misc. brick events that the application doesn't use"
    $ (forAllEnvs @App.Config)
    $ (throwingErrors @App.Error)
    $ ignoreLogging
    $ withMockGraphite
    $ do
        event      <- pick $ oneof [mouseDown, mouseUp]
        startState <- pick arbitrary
        state'     <- pick arbitrary
        let handler = E.EventHandler { continue = const (return Nothing)
                                     , ignore   = const (return $ Just (Right state'))
                                     , stop     = const (return Nothing)
                                     }
        result <- run (E.handleEvent handler event (Right startState))
        case result of
          Just (Right result') -> return (result' === state')
          _                    -> return (property False)

  appProp "updates the app state from graphite when requested (UpdateEvent)"
    $ (forAllEnvs @App.Config)
    $ (throwingErrors @App.Error)
    $ ignoreLogging
    $ withMockGraphite
    $ do
        (_, update) <- lift get
        result      <- run E.updateGraphData
        return (result `shouldBe` Graph.extractGraph update)

  appProp "ends the event loop when an Exit Key is pressed"
    $ (forAllEnvs @App.Config)
    $ (throwingErrors @App.Error)
    $ ignoreLogging
    $ withMockGraphite
    $ do
        startState <- pick arbitrary
        state'     <- pick arbitrary
        let handler = E.EventHandler { continue = const (return Nothing)
                                     , ignore   = const (return Nothing)
                                     , stop     = const (return $ Just (Right state'))
                                     }
        result <- run (E.handleEvent handler (VtyEvent E.ExitKey) (Right startState))
        case result of
          Just (Right result') -> return (result' === state')
          _                    -> return (property False)

  appProp "returns errors as part of the state"
    $ (forAllEnvs @App.Config)
    $ (throwingErrors @App.Error)
    $ ignoreLogging
    $ withFailingGraphite
    $ do
        startState <- pick arbitrary
        let
          handler =
            E.EventHandler { continue = return . Just, ignore = const (return Nothing), stop = const (return Nothing) }
        result <- run (E.handleEvent handler (Brick.AppEvent E.UpdateEvent) (Right startState))
        case result of
          Just (Left _) -> return (property True)
          _             -> return (property False)
