{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PatternSynonyms #-}

module App.State where

import           Control.Lens.TH
import           Graphite.Types
import           Data.Time.LocalTime
import qualified App.Config                    as App
import qualified Brick.Widgets.List            as BWL
import           App.Components
import           Control.Lens.Prism
import           Control.Lens.Wrapped
import           Display.Graph                 as Graph
import           Control.Monad.Except           ( MonadError(catchError) )
import App.State as State

newtype Error = AppGraphiteError GraphiteError
  deriving ( Show, Generic )
  deriving anyclass Exception

makePrisms ''Error

type MetricsView = BWL.List AppComponent Metric

data ActiveState = ActiveState {
     _metricsView :: Maybe MetricsView
   , _graphData :: Graph Time Value
   , _timezone ::  !TimeZone
} deriving ( Show , Generic )

makeLenses ''ActiveState

class GraphViewer m where
  updateGraph :: m (Graph Time Value) -> CurrentState -> m CurrentState

toggleMetricsView :: CurrentState -> m CurrentState
toggleMetricsView = traverseOf (App.active . App.metricsView) $ return . \case
  Nothing -> (Just (BWL.list MetricsBrowserComponent (fromList ["example", "other.example"]) 1))
  _       -> Nothing

newtype FailedState = FailedState { failure :: Error }
  deriving ( Show, Generic )

newtype CurrentState = CurrentState ( Either FailedState ActiveState )
  deriving ( Generic , Show )

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

constructDefaultContext :: MonadIO m => App.Config -> m ActiveState
constructDefaultContext _ = do
  _timezone <- liftIO (getTimezone `catchError` defaultToUtc)
  let _metricsView = Nothing
  let _graphData   = mempty
  return (ActiveState { .. })
 where
  getTimezone  = liftIO getCurrentTimeZone
  defaultToUtc = const (pure utc)
