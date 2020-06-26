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
import qualified Brick.Widgets.List as Brick
import Control.Lens.Combinators
import Data.Time.LocalTime
import Data.Vector (Vector)
import Display.Graph as Graph
import Events.Types
import Graphite.Types

newtype Error = AppGraphiteError GraphiteError
  deriving (Show, Generic)
  deriving anyclass (Exception)

data GraphData = Missing | Pending | Present (Graph Time Value)
  deriving (Eq, Show, Generic)

data ActiveState t
  = ActiveState
      { _metricsView :: Maybe (MetricsBrowserWidget' t),
        _graphData :: !GraphData,
        _timezone :: !TimeZone
      }
  deriving (Generic)

makeLenses ''ActiveState

newtype FailedState = FailedState {failure :: Error}
  deriving (Show, Generic)

newtype CurrentState' t = CurrentState (Either FailedState (ActiveState t))
  deriving (Generic)

type CurrentState = CurrentState' (Brick.GenericList AppComponent Vector)

instance Wrapped (CurrentState' t)

active :: Prism' (CurrentState' t) (ActiveState t)
active = _Wrapped' . _Right

pattern Active :: ActiveState t -> CurrentState' t
pattern Active s = CurrentState (Right s)

pattern Failed :: FailedState -> CurrentState' t
pattern Failed s = CurrentState (Left s)

{-# COMPLETE Active, Failed #-}

constructDefaultContext :: MonadIO m => Brick.BChan AppEvent -> App.Config -> m (ActiveState t)
constructDefaultContext _eventCh _ = do
  _timezone <- liftIO (getTimezone `catchError` defaultToUtc)
  let _metricsView = Nothing
  let _graphData = Missing
  return (ActiveState {..})
  where
    getTimezone = liftIO getCurrentTimeZone
    defaultToUtc = const (pure utc)
