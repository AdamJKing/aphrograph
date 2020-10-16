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
  ( GraphViewer,
    MetricsBrowserWidget (ClosedMetricsBrowser, OpenMetricsBrowser),
    close,
    open,
    targeted,
    update,
    updateGraph,
  )
import qualified App.State as App
import qualified Brick as Brick
import Control.Lens (traverseOf, (%~), (.~), (^.))
import Display.Graph (Graph)
import qualified Display.Graph as Graph (extract, mkGraph)
import Display.GraphWidget (GraphDisplay (NoDataDisplay), graphDisplay, graphiteRequest)
import Events.Types
  ( AppEvent (..),
    MetricsBrowserEvent (Modify),
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

keyPressHandler :: (MonadEvent AppEvent n, MonadGraphite m) => (forall x. n x -> m x) -> EventHandler m Vty.Event (App.CurrentState m)
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
      newState <- cm & traverseOf (App._Active . App.metricsView) (handleMiscEvents ?? otherKeyPress)
      return (Continue, newState)
  where
    selectMetric activeState =
      activeState
        & traverseOf
          App.graphData
          ( \gd ->
              case targeted (activeState ^. App.metricsView) of
                Nothing -> return (gd & graphDisplay .~ NoDataDisplay)
                Just newTargetMetric -> do
                  nat (writeEvent TriggerUpdate)
                  return
                    ( gd & graphiteRequest %~ (\gr -> gr {requestMetric = newTargetMetric})
                    )
          )
        <&> App.metricsView %~ close

    toggleMetricsBrowser (ClosedMetricsBrowser {open}) = listMetrics <&> open
    toggleMetricsBrowser (OpenMetricsBrowser {close}) = return close

    handleMiscEvents (closed@(ClosedMetricsBrowser _)) _ = return closed
    handleMiscEvents (OpenMetricsBrowser {update}) otherKeyPress = update (Modify otherKeyPress)

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
