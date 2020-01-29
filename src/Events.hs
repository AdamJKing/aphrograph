{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}

module Events where

import qualified Brick.Types                   as Brick
import qualified Graphics.Vty.Input.Events     as Vty
import qualified App.Config                    as App
import qualified App.State                     as App
import           Graphite.Types
import           App.Components
import qualified Display.Graph                 as Graph

data AppEvent = UpdateEvent
    deriving ( Show, Eq )

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

class MonadOutcome (EventF m) m => MonadEventHandler e m | m -> e where
  type EventS m
  handleEvent :: e -> EventS m -> m (EventF m (EventS m))

handleKeyPress :: (MonadOutcome f m) => Vty.Event -> App.CurrentState -> m (f App.CurrentState)
handleKeyPress (KeyDown 'm') = continue <=< App.toggleMetricsView
handleKeyPress ExitKey       = stop
handleKeyPress _             = continue

handleBrickEvent
  :: (GraphViewer m, EventS m ~ App.CurrentState, MonadEventHandler AppEvent m, App.Configured App.Config m, MonadGraphite m)
  => Brick.BrickEvent n AppEvent
  -> App.CurrentState
  -> m (EventF m App.CurrentState)
handleBrickEvent (Brick.VtyEvent keyPress) = handleKeyPress keyPress
handleBrickEvent (Brick.AppEvent appEvent) = handleEvent appEvent
handleBrickEvent _                         = continue

class GraphViewer m where
  updateGraph :: m (Graph.Graph Time Value)