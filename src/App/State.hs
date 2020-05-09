{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module App.State where

import App.Components
import qualified App.Config as App
import qualified Brick.BChan as Brick
import qualified Brick.Widgets.List as BWL
import Control.Lens.Combinators
import Data.Time.LocalTime
import Display.Graph as Graph
import Events.Types
import Graphite.Types

newtype Error = AppGraphiteError GraphiteError
  deriving (Show, Generic)
  deriving anyclass (Exception)

type MetricsView = BWL.List AppComponent Metric

data GraphData = Missing | Pending | Present (Graph Time Value)
  deriving (Eq, Show, Generic)

data ActiveState
  = ActiveState
      { _metricsView :: Maybe MetricsView,
        _graphData :: !GraphData,
        _timezone :: !TimeZone
      }
  deriving (Generic, Show)

makeLenses ''ActiveState

updateGraph :: Applicative f => f (Graph.Graph Time Value) -> CurrentState -> f CurrentState
updateGraph update = traverseOf (active . graphData) (\_ -> Present <$> update)

setMetricsView :: MonadGraphite m => (MetricsView -> m MetricsView) -> CurrentState -> m CurrentState
setMetricsView update = traverseOf (active . metricsView) $ \case
  (Just mv) -> Just <$> update mv
  _ -> return Nothing

toggleMetricsView :: MonadGraphite m => CurrentState -> m CurrentState
toggleMetricsView = traverseOf (active . metricsView) $ \case
  Nothing -> do
    availableMetrics <- take 10 <$> listMetrics
    return (Just (BWL.list MetricsBrowserComponent (fromList availableMetrics) 1))
  _ -> return Nothing

newtype FailedState = FailedState {failure :: Error}
  deriving (Show, Generic)

newtype CurrentState = CurrentState (Either FailedState ActiveState)
  deriving (Generic)

instance Wrapped CurrentState

active :: Prism' CurrentState ActiveState
active = _Wrapped' . _Right

failed :: Prism' CurrentState FailedState
failed = _Wrapped' . _Left

pattern Active :: ActiveState -> CurrentState
pattern Active s = CurrentState (Right s)

pattern Failed :: FailedState -> CurrentState
pattern Failed s = CurrentState (Left s)

{-# COMPLETE Active, Failed #-}

constructDefaultContext :: MonadIO m => Brick.BChan AppEvent -> App.Config -> m ActiveState
constructDefaultContext _eventCh _ = do
  _timezone <- liftIO (getTimezone `catchError` defaultToUtc)
  let _metricsView = Nothing
  let _graphData = Missing
  return (ActiveState {..})
  where
    getTimezone = liftIO getCurrentTimeZone
    defaultToUtc = const (pure utc)
