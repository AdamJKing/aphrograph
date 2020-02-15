{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module App.State
  ( metricsView,
    renderData,
    appData,
    timezone,
    graphData,
    toggleMetricsView,
    setMetricsView,
    updateGraph,
    constructDefaultContext,
    CurrentState (..),
    ActiveState (..),
    FailedState (..),
    Error (..),
  )
where

import App.Components
import qualified App.Config as App
import qualified Brick.Widgets.List as BWL
import Control.Lens.Combinators
import Control.Monad.Except (MonadError (catchError))
import Data.Time.LocalTime
import Display.Graph as Graph
import Graphite.Types

newtype Error = AppGraphiteError GraphiteError
  deriving (Show, Generic)
  deriving anyclass (Exception)

makePrisms ''Error

type MetricsView = BWL.List AppComponent Metric

data ActiveState
  = ActiveState
      { _metricsView :: Maybe MetricsView,
        _graphData :: Graph Time Value,
        _timezone :: !TimeZone
      }
  deriving (Show, Generic)

makeLenses ''ActiveState

newtype FailedState = FailedState {failure :: Error}
  deriving (Show, Generic)

data CurrentState
  = CurrentState
      { _appData :: Either FailedState ActiveState,
        _renderData :: DisplayWidget Error
      }
  deriving (Generic, Show)

makeLenses ''CurrentState

updateGraph :: Applicative f => f (Graph.Graph Time Value) -> CurrentState -> f CurrentState
updateGraph update = traverseOf (appData . _Right . graphData) (const update)

setMetricsView :: MonadGraphite m => (MetricsView -> m MetricsView) -> CurrentState -> m CurrentState
setMetricsView update = traverseOf (appData . _Right . metricsView) $ \case
  (Just mv) -> Just <$> update mv
  _ -> return Nothing

toggleMetricsView :: MonadGraphite m => CurrentState -> m CurrentState
toggleMetricsView = traverseOf (appData . _Right . metricsView) $ \case
  Nothing -> do
    availableMetrics <- take 10 <$> listMetrics
    return (Just (BWL.list MetricsBrowserComponent (fromList availableMetrics) 1))
  _ -> return Nothing

constructDefaultContext :: MonadIO m => App.Config -> m ActiveState
constructDefaultContext _ = do
  _timezone <- liftIO (getTimezone `catchError` defaultToUtc)
  let _metricsView = Nothing
  let _graphData = mempty
  let _renderData = DisplayWidget $ Right $ DefaultDisplay NoDataDisplayWidget Nothing
  return (ActiveState {..})
  where
    getTimezone = liftIO getCurrentTimeZone
    defaultToUtc = const (pure utc)
