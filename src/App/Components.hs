{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module App.Components where

import qualified Brick.Widgets.List as Brick
import Data.Time.LocalTime
import Display.Graph
import qualified Graphite.Types as Graphite

data HorizontalAxisWidget = HorizontalAxis [Graphite.Time] TimeZone deriving (Show, Generic)

newtype VerticalAxisWidget = VerticalAxis [Graphite.Value] deriving (Show, Generic)

newtype GraphCanvasWidget = GraphCanvas (Graph Graphite.Time Graphite.Value) deriving (Show, Generic)

data GraphDisplayWidget = GraphDisplay GraphCanvasWidget VerticalAxisWidget HorizontalAxisWidget | NoDataDisplayWidget deriving (Show, Generic)

newtype ErrorWidget e = ErrorWidget e deriving (Show, Generic)

data AppComponent = GraphView | MetricsBrowserComponent
  deriving (Eq, Ord, Show, Generic)

newtype MetricsBrowserWidget = MetricsBrowser (Brick.List AppComponent Graphite.Metric)
  deriving (Show, Generic)

data AppWidget
  = DefaultDisplay
      { dataDisplay :: !GraphDisplayWidget,
        metricBrowser :: Maybe MetricsBrowserWidget
      }
  deriving (Show, Generic)

newtype DisplayWidget e = DisplayWidget (Either (ErrorWidget e) AppWidget)
  deriving (Show, Generic)
