{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Events where

import App.Components
import qualified App.Config as App
import qualified App.State as App
import qualified Brick.Types as Brick
import qualified Display.Graph as Graph
import qualified Graphics.Vty.Input.Events as Vty
import Graphite.Types

data AppEvent = UpdateEvent
  deriving (Show, Eq)

type SystemEvent = Brick.BrickEvent AppComponent AppEvent

pattern KeyDown :: Char -> Vty.Event
pattern KeyDown k = Vty.EvKey (Vty.KChar k) []

pattern ExitKey :: Vty.Event
pattern ExitKey = KeyDown 'q'

data EventOutcome = Continue | Ignore | Stop
  deriving (Show, Eq, Generic)

type family EventF (m :: Type -> Type) :: Type -> Type

type instance EventF (Brick.EventM n) = Brick.Next

class Monad m => MonadOutcome f (m :: Type -> Type) where
  continue :: a -> m (f a)
  stop :: a -> m (f a)

class MonadOutcome (EventF m) m => MonadEventHandler e m where
  type EventS m
  handleEvent :: e -> EventS m -> m (EventF m (EventS m))

handleBrickEvent ::
  ( GraphViewer m,
    EventS m ~ App.CurrentState,
    MonadEventHandler AppEvent m,
    App.Configured App.Config m,
    MonadGraphite m,
    MonadEventHandler Vty.Event m
  ) =>
  Brick.BrickEvent n AppEvent ->
  App.CurrentState ->
  m (EventF m App.CurrentState)
handleBrickEvent (Brick.VtyEvent keyPress) = handleEvent keyPress
handleBrickEvent (Brick.AppEvent appEvent) = handleEvent appEvent
handleBrickEvent _ = continue

class GraphViewer m where
  updateGraph :: m (Graph.Graph Time Value)
