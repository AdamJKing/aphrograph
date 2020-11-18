{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Events where

import App (AppM, AppT)
import App.Components
  ( Browsable (..),
    ComponentM,
    ComponentName,
    Dialogue (..),
    GraphViewer (..),
    chosenTimeOffset,
    updateDialogue,
    updateGraph,
    _OpenOnMetrics,
    _OpenOnTime,
  )
import qualified App.State as App
import qualified Brick
import Control.Lens (over, traverseOf, view, (%~), (.~), (^.), (^?))
import Control.Monad.Morph (MFunctor (hoist))
import Display.Graph (Graph)
import qualified Display.Graph as Graph (extract, mkGraph)
import Display.GraphWidget (GraphDisplay (NoDataDisplay), graphDisplay, graphiteRequest)
import Display.TimeDialogueWidget (timeDialogue)
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
  EventHandler AppM Vty.Event (App.CurrentState e) ->
  EventHandler AppM e (App.CurrentState e) ->
  EventHandler AppM (Brick.BrickEvent ComponentName e) (App.CurrentState e)
brickEventHandler handleKeyPresses handleAppEvents e s = do
  intermediate <- traverseOf (App._Active . App.dialogue . _OpenOnTime) (lift . updateDialogue e) s
  case e of
    (Brick.VtyEvent keyPress) -> handleKeyPresses keyPress intermediate
    (Brick.AppEvent appEvent) -> handleAppEvents appEvent intermediate
    _unhandled -> return (Continue, intermediate)

keyPressHandler :: EventHandler (AppT ComponentM) Vty.Event (App.CurrentState e)
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
    selectMetric :: App.ActiveState e -> AppT ComponentM (App.ActiveState e)
    selectMetric activeState =
      activeState
        & traverseOf
          App.graphData
          ( \gd ->
              case activeState ^? App.dialogue . _OpenOnMetrics of
                Nothing -> return gd
                Just mb ->
                  case selected mb of
                    Nothing -> return (gd & graphDisplay .~ NoDataDisplay)
                    Just newTargetMetric -> do
                      hoist liftIO (writeEvent TriggerUpdate)
                      return
                        ( gd & graphiteRequest %~ (\gr -> gr {requestMetric = newTargetMetric})
                        )
          )
        <&> App.dialogue .~ Closed

    toggleMetricsBrowser :: MonadIO m => Dialogue n e -> AppT m (Dialogue n e)
    toggleMetricsBrowser (OpenOnMetrics _) = return Closed
    toggleMetricsBrowser (OpenOnTime _) = toggleMetricsBrowser Closed
    toggleMetricsBrowser Closed = OpenOnMetrics . open <$> listMetrics

    toggleTimeDialogue :: App.ActiveState e -> App.ActiveState e
    toggleTimeDialogue = do
      previous <- view (App.componentState . chosenTimeOffset)
      over App.dialogue $ \case
        (OpenOnTime _) -> Closed
        _dialogueClosed -> (OpenOnTime (timeDialogue previous))

    handleMiscEvents :: Vty.Event -> Dialogue n e -> AppT ComponentM (Dialogue n e)
    handleMiscEvents otherKeyPress = traverseOf _OpenOnMetrics (lift . scroll otherKeyPress)

appEventHandler :: EventHandler (AppT ComponentM) AppEvent (App.CurrentState e)
appEventHandler graphUpdate priorState = do
  updated <- update priorState
  return (Continue, updated)
  where
    update = traverseOf (App._Active . App.graphData) $ \gw ->
      case graphUpdate of
        (GraphUpdate newGraph) ->
          lift (updateGraph (gw ^. graphiteRequest) newGraph)
        TriggerUpdate -> hoist liftIO (refresh (gw ^. graphiteRequest) $> gw)

refresh :: (MonadGraphite m, MonadEvent AppEvent m) => GraphiteRequest -> m ()
refresh req = writeEventLater (GraphUpdate <$> fetchGraph req)

fetchGraph :: MonadGraphite m => GraphiteRequest -> m (Graph Time Value)
fetchGraph request = do
  datapoints <- getMetrics request
  return (toGraph datapoints)
  where
    toGraph = Graph.mkGraph . fmap Graph.extract
