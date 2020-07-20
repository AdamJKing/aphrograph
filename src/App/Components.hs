{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module App.Components where

import qualified Brick.Widgets.List as Brick
import Data.Time.LocalTime
import Data.Vector (Vector)
import Display.Graph
import qualified Graphics.Vty.Input.Events as Vty
import qualified Graphite.Types as Graphite
import Relude

data HorizontalAxisWidget = HorizontalAxis [Graphite.Time] TimeZone deriving (Show)

newtype VerticalAxisWidget = VerticalAxis [Graphite.Value] deriving (Show)

newtype GraphCanvasWidget = GraphCanvas (Graph Graphite.Time Graphite.Value) deriving (Show)

data GraphDisplayWidget = GraphDisplay GraphCanvasWidget VerticalAxisWidget HorizontalAxisWidget | LoadingDataDisplayWidget | NoDataDisplayWidget deriving (Show)

newtype ErrorWidget e = ErrorWidget e deriving (Show)

data AppComponent = GraphView | MetricsBrowserComponent
  deriving (Eq, Ord, Show)

newtype MetricsBrowserWidget' t = MetricsBrowser (t Graphite.Metric) deriving (Generic)

type MetricsBrowserWidget = MetricsBrowserWidget' (Brick.GenericList AppComponent Vector)

metricBrowserWidget :: [Graphite.Metric] -> MetricsBrowserWidget
metricBrowserWidget = MetricsBrowser . (Brick.list MetricsBrowserComponent ?? 1) . fromList

class MetricsBrowser m where
  updateMetricBrowserWidget :: Vty.Event -> MetricsBrowserWidget -> m MetricsBrowserWidget

data AppWidget e
  = DefaultDisplay
      { dataDisplay :: !GraphDisplayWidget,
        metricBrowser :: Maybe MetricsBrowserWidget
      }

newtype DisplayWidget e = DisplayWidget (Either (ErrorWidget e) (AppWidget e))
