{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Events where

import App
import App.Components
import App.Config as App
import qualified App.State as App
import qualified Brick.BChan as Brick
import qualified Brick.Main as Brick
import qualified Brick.Types as Brick
import qualified Brick.Widgets.List as Brick
import Control.Concurrent.Lifted
import Control.Lens.Getter
import Control.Lens.Operators
import Control.Monad.Base
import Control.Monad.Morph
import Control.Monad.Trans.Control
import Data.Has
import Display.Graph as Graph
import Events.Types
import qualified Graphics.Vty.Input.Events as Vty
import Graphite.Types

pattern KeyDown :: Char -> Vty.Event
pattern KeyDown k = Vty.EvKey (Vty.KChar k) []

pattern ExitKey :: Vty.Event
pattern ExitKey = KeyDown 'q'

newtype EventT m a = MkEventT (ReaderT App.CurrentState (ExceptT App.Error m) a)
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadIO,
      MonadReader App.CurrentState,
      MonadError App.Error
    )

instance MonadOutcome m => MonadOutcome (EventT m) where
  type EventF (EventT m) = EventF m

deriving instance MonadBase b m => MonadBase b (EventT m)

deriving instance MonadBaseControl b m => MonadBaseControl b (EventT m)

instance MFunctor EventT where
  hoist f (MkEventT action) = MkEventT $ hoist (hoist f) action

type EventM = EventT (Brick.EventM AppComponent)

instance MonadTrans EventT where
  lift = MkEventT . lift . lift

instance MonadIO m => GraphViewer (EventT m) where
  updateGraph = do
    App.GraphiteConfig {..} <- lift (view (App.config . App.graphiteConfig))
    datapoints <- getMetrics $ RenderRequest _fromTime _toTime _targetArg
    return (Graph.mkGraph $ Graph.extract <$> datapoints)

handleNavigationEvents :: MonadBase (Brick.EventM AppComponent) f => MetricsBrowserWidget -> Vty.Event -> f MetricsBrowserWidget
handleNavigationEvents (VisibleMetricsBrowser _) (KeyDown 'm') = return InvisibleMetricsBrowser
handleNavigationEvents InvisibleMetricsBrowser (KeyDown 'm') = do
  metrics <- listMetrics
  return (VisibleMetricsBrowser (Brick.list MetricsBrowserComponent 1 _))
handleNavigationEvents (VisibleMetricsBrowser browser) keyPress =
  metricsBrowserWidget <$> liftBase (Brick.handleListEventVi Brick.handleListEvent keyPress browser)

instance MonadEventHandler Vty.Event App.CurrentState (EventT m) where
  handleEvent ExitKey previousState = stop previousState
  handleEvent (KeyDown 'm') previousState = updateMetricsView _
  handleEvent otherKeyPress previousState = updateMetricsView (Brick.handleListEventVi Brick.handleListEvent otherKeyPress (view (App.active . App.metricsView) previousState))

instance MonadEventHandler AppEvent App.CurrentState (EventT (AppT (Brick.EventM AppComponent))) where
  handleEvent (GraphUpdate newGraph) st = do
    lift . lift $ Brick.invalidateCacheEntry GraphView
    let newState = st & (App.active . App.graphData) .~ App.Present newGraph
    continue newState
  handleEvent TriggerUpdate (App.Active previousState) = do
    lift (hoist liftIO triggerUpdate)
    continue (App.Active (previousState & App.graphData .~ App.Pending))
  handleEvent _ previousState = continue previousState

instance MonadEventHandler (Brick.BrickEvent n AppEvent) App.CurrentState (EventT (AppT (Brick.EventM AppComponent))) where
  handleEvent (Brick.VtyEvent keyPress) = handleEvent keyPress
  handleEvent (Brick.AppEvent appEvent) = handleEvent appEvent
  handleEvent _ = continue

writeEvent :: (Has (Brick.BChan AppEvent) r, MonadReader r m, MonadIO m) => e -> m ()
writeEvent e = do
  ch <- view getter
  liftIO (Brick.writeBChan ch e)

triggerUpdate :: EventT (AppT IO) ()
triggerUpdate = do
  void $ fork $ do
    newGraph <- updateGraph
    writeEvent (GraphUpdate newGraph)
