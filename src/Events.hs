{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Events where

import App.Components
  ( ComponentName (MetricsView),
    GraphViewer,
    updateGraph,
  )
import qualified App.State as App
import qualified Brick as Brick
import qualified Brick.Widgets.List as Brick
import Control.Lens (traverseOf, (%~), (.~), (^.))
import Control.Monad.Base (MonadBase (liftBase))
import Display.Graph (Graph)
import qualified Display.Graph as Graph (extract, mkGraph)
import Display.GraphWidget (GraphDisplay (NoDataDisplay), graphDisplay, graphiteRequest)
import Events.Types
  ( AppEvent (..),
    MonadEvent,
    writeEvent,
    writeEventLater,
  )
import qualified Graphics.Vty.Input.Events as Vty
import Graphite.Types
  ( GraphiteRequest,
    MonadGraphite,
    Time,
    Value,
    getMetrics,
    listMetrics,
    requestMetric,
  )

pattern KeyDown :: Char -> Vty.Event
pattern KeyDown k = Vty.EvKey (Vty.KChar k) []

pattern EnterKey :: Vty.Event
pattern EnterKey = Vty.EvKey Vty.KEnter []

pattern ExitKey :: Vty.Event
pattern ExitKey = KeyDown 'q'

data EventOutcome = Continue | Halt

type EventHandler m e s = e -> s -> m (EventOutcome, s)

brickEventHandler ::
  Applicative m =>
  EventHandler m Vty.Event s ->
  EventHandler m e s ->
  EventHandler m (Brick.BrickEvent n e) s
brickEventHandler handleKeyPresses handleAppEvents = \case
  (Brick.VtyEvent keyPress) -> handleKeyPresses keyPress
  (Brick.AppEvent appEvent) -> handleAppEvents appEvent
  _unhandled -> pure . (Continue,)

keyPressHandler :: (MonadEvent AppEvent n, MonadGraphite m, MonadBase (Brick.EventM ComponentName) m) => (forall x. n x -> m x) -> EventHandler m Vty.Event (App.CurrentState m)
keyPressHandler nat = \event cm ->
  case event of
    ExitKey -> return (Halt, cm)
    EnterKey -> do
      newState <- cm & traverseOf App._Active selectMetric
      return (Continue, newState)
    KeyDown 'm' -> do
      newState <- cm & traverseOf (App._Active . App.metricsView) toggleMetricsBrowser
      return (Continue, newState)
    otherKeyPress -> do
      newState <- cm & traverseOf (App._Active . App.metricsView) (handleMiscEvents otherKeyPress)
      return (Continue, newState)
  where
    selectMetric activeState =
      activeState
        & traverseOf
          App.graphData
          ( \gd ->
              case (activeState ^. App.metricsView) >>= Brick.listSelectedElement of
                Nothing -> return (gd & graphDisplay .~ NoDataDisplay)
                Just (_, newTargetMetric) -> do
                  nat (writeEvent TriggerUpdate)
                  return
                    ( gd & graphiteRequest %~ (\gr -> gr {requestMetric = newTargetMetric})
                    )
          )
        <&> App.metricsView .~ Nothing

    toggleMetricsBrowser (Just _browser) = return Nothing
    toggleMetricsBrowser Nothing = do
      metrics <- listMetrics
      return $ Just (Brick.list MetricsView metrics 1)

    handleMiscEvents otherKeyPress = traverse $ \browser ->
      liftBase (Brick.handleListEventVi Brick.handleListEvent otherKeyPress browser)

appEventHandler :: (GraphViewer m, MonadGraphite n, MonadEvent AppEvent n) => (forall x. n x -> m x) -> EventHandler m AppEvent (App.CurrentState o)
appEventHandler nat graphUpdate priorState = do
  updated <- update priorState
  return (Continue, updated)
  where
    update = traverseOf (App._Active . App.graphData) $ \gw ->
      case graphUpdate of
        (GraphUpdate newGraph) ->
          updateGraph (gw ^. graphiteRequest) newGraph
        TriggerUpdate -> (nat $ refresh (gw ^. graphiteRequest)) >> return gw

refresh :: (MonadGraphite m, MonadEvent AppEvent m) => GraphiteRequest -> m ()
refresh req = writeEventLater (GraphUpdate <$> fetchGraph req)

fetchGraph :: MonadGraphite m => GraphiteRequest -> m (Graph Time Value)
fetchGraph request = do
  datapoints <- getMetrics $ request
  return (toGraph datapoints)
  where
    toGraph = Graph.mkGraph . fmap Graph.extract
