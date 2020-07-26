{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module App.Components where

import Control.Lens
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

data MetricsBrowserWidget' t = MetricsBrowser {metrics :: (t Graphite.Metric), width :: Int} deriving (Generic)

type MetricsBrowserWidget = MetricsBrowserWidget' (Brick.GenericList AppComponent Vector)

metricBrowserWidget :: Vector Graphite.Metric -> MetricsBrowserWidget
metricBrowserWidget metrics =
  MetricsBrowser
    { metrics = (Brick.list MetricsBrowserComponent metrics 1),
      width = fromMaybe 25 (maximumOf (traverse . to Graphite.metricLength) metrics)
    }

class MetricsBrowser m where
  updateMetricBrowserWidget :: Vty.Event -> MetricsBrowserWidget -> m MetricsBrowserWidget

data AppWidget e = DefaultDisplay
  { dataDisplay :: !GraphDisplayWidget,
    metricBrowser :: Maybe MetricsBrowserWidget
  }

newtype DisplayWidget e = DisplayWidget (Either (ErrorWidget e) (AppWidget e))
