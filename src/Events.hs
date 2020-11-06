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

import App (AppT)
import App.Components
  ( Browsable (..),
    ComponentM,
    GraphViewer (..),
    MetricsBrowser,
    updateGraph,
  )
import qualified App.State as App
import qualified Brick as Brick
import Control.Lens (traverseOf, (%~), (.~), (^.))
import Control.Monad.Morph (MFunctor (hoist))
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

keyPressHandler :: EventHandler (AppT ComponentM) Vty.Event App.CurrentState
keyPressHandler event cm =
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
    selectMetric :: App.ActiveState -> AppT ComponentM App.ActiveState
    selectMetric activeState =
      activeState
        & traverseOf
          App.graphData
          ( \gd ->
              case (activeState ^. App.metricsView) >>= selected of
                Nothing -> return (gd & graphDisplay .~ NoDataDisplay)
                Just newTargetMetric -> do
                  hoist liftIO (writeEvent TriggerUpdate)
                  return
                    ( gd & graphiteRequest %~ (\gr -> gr {requestMetric = newTargetMetric})
                    )
          )
        <&> App.metricsView .~ Nothing

    toggleMetricsBrowser :: MonadIO m => Maybe a -> AppT m (Maybe MetricsBrowser)
    toggleMetricsBrowser (Just _browser) = return Nothing
    toggleMetricsBrowser Nothing = do
      metrics <- listMetrics
      return $ Just (open metrics)

    handleMiscEvents :: Vty.Event -> Maybe MetricsBrowser -> AppT ComponentM (Maybe MetricsBrowser)
    handleMiscEvents otherKeyPress = traverse (lift . scroll otherKeyPress)

appEventHandler :: EventHandler (AppT ComponentM) AppEvent App.CurrentState
appEventHandler graphUpdate priorState = do
  updated <- update priorState
  return (Continue, updated)
  where
    update = traverseOf (App._Active . App.graphData) $ \gw ->
      case graphUpdate of
        (GraphUpdate newGraph) ->
          lift (updateGraph (gw ^. graphiteRequest) newGraph)
        TriggerUpdate -> hoist liftIO $ (refresh (gw ^. graphiteRequest) $> gw)

refresh :: (MonadGraphite m, MonadEvent AppEvent m) => GraphiteRequest -> m ()
refresh req = writeEventLater (GraphUpdate <$> fetchGraph req)

fetchGraph :: MonadGraphite m => GraphiteRequest -> m (Graph Time Value)
fetchGraph request = do
  datapoints <- getMetrics $ request
  return (toGraph datapoints)
  where
    toGraph = Graph.mkGraph . fmap Graph.extract
