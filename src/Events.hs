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
    TimeDialogue,
    chosenTimeOffset,
    timeDialogue,
    updateDialogue,
    updateGraph,
  )
import qualified App.State as App
import qualified Brick as Brick
import Control.Lens (over, traverseOf, (%~), (.~), (^.), (^?), _Just, _Left)
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
      newState <- cm & traverseOf (App._Active . App.dialogue) toggleMetricsBrowser
      return (Continue, newState)
    KeyDown 't' -> return (Continue, cm & over App._Active toggleTimeDialogue)
    otherKeyPress -> do
      newState <- cm & traverseOf (App._Active . App.dialogue) (handleMiscEvents otherKeyPress)
      return (Continue, newState)
  where
    selectMetric :: App.ActiveState -> AppT ComponentM App.ActiveState
    selectMetric activeState =
      activeState
        & traverseOf
          App.graphData
          ( \gd ->
              case (activeState ^? App.dialogue . _Just . _Left) >>= selected of
                Nothing -> return (gd & graphDisplay .~ NoDataDisplay)
                Just newTargetMetric -> do
                  hoist liftIO (writeEvent TriggerUpdate)
                  return
                    ( gd & graphiteRequest %~ (\gr -> gr {requestMetric = newTargetMetric})
                    )
          )
        <&> App.dialogue .~ Nothing

    toggleMetricsBrowser :: MonadIO m => Maybe (Either MetricsBrowser TimeDialogue) -> AppT m (Maybe (Either MetricsBrowser TimeDialogue))
    toggleMetricsBrowser (Just (Left _browser)) = return Nothing
    toggleMetricsBrowser _browserNotOpen = do
      metrics <- listMetrics
      return $ Just $ Left (open metrics)

    toggleTimeDialogue :: App.ActiveState -> App.ActiveState
    toggleTimeDialogue active =
      case active ^. App.dialogue of
        (Just (Right _openDialogue)) -> active & App.dialogue .~ Nothing
        _otherState ->
          let previous = active ^. (App.componentState . chosenTimeOffset)
           in active & App.dialogue .~ (Just $ Right $ timeDialogue previous)

    handleMiscEvents :: Vty.Event -> Maybe (Either MetricsBrowser TimeDialogue) -> AppT ComponentM (Maybe (Either MetricsBrowser TimeDialogue))
    handleMiscEvents _ Nothing = return Nothing
    handleMiscEvents otherKeyPress (Just dialogueState) =
      lift $
        Just <$> case dialogueState of
          Right td -> Right <$> updateDialogue otherKeyPress td
          Left mv -> Left <$> scroll otherKeyPress mv

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
