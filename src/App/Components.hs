{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module App.Components where

import qualified Brick as Brick
import qualified Brick.Widgets.Border as WidgetB
import qualified Brick.Widgets.Center as Widget
import qualified Brick.Widgets.List as BWL
import Control.Lens (maximumOf, to, (^.))
import Control.Monad.Base (MonadBase (liftBase))
import Data.Time.LocalTime (TimeZone)
import Data.Vector (Vector)
import Display.Graph (Graph)
import Events.Types (MetricsBrowserEvent (Modify))
import qualified Graphite.Types as Graphite

data HorizontalAxisWidget = HorizontalAxis [Graphite.Time] TimeZone deriving (Show, Generic)

newtype VerticalAxisWidget = VerticalAxis [Graphite.Value] deriving (Show, Generic)

newtype GraphCanvasWidget = GraphCanvas (Graph Graphite.Time Graphite.Value) deriving (Show, Generic)

data GraphDisplayWidget
  = GraphDisplay GraphCanvasWidget VerticalAxisWidget HorizontalAxisWidget
  | LoadingDataDisplayWidget
  | NoDataDisplayWidget
  deriving (Show, Generic)

newtype ErrorWidget e = ErrorWidget e deriving (Show)

data ComponentName = GraphView | MetricsView
  deriving (Eq, Ord, Show)

data MetricsBrowserWidget m
  = ClosedMetricsBrowser {open :: Vector Graphite.Metric -> MetricsBrowserWidget m}
  | OpenMetricsBrowser {update :: MetricsBrowserEvent -> m (MetricsBrowserWidget m), close :: MetricsBrowserWidget m, display :: Brick.Widget ComponentName}
  deriving (Generic)

class Monad m => GraphViewer m where
  triggerUpdate :: Graphite.Metric -> m ()
  updateGraph :: Graph Graphite.Time Graphite.Value -> m GraphDisplayWidget

data AppWidget m = DefaultDisplay
  { dataDisplay :: GraphDisplayWidget,
    metricBrowser :: MetricsBrowserWidget m
  }

newtype DisplayWidget m e = DisplayWidget (Either (ErrorWidget e) (AppWidget m))

closedMetricsBrowser :: MonadBase (Brick.EventM ComponentName) m => MetricsBrowserWidget m
closedMetricsBrowser = ClosedMetricsBrowser {open}
  where
    open metrics = openMetricsBrowser (BWL.list MetricsView metrics 1)

openMetricsBrowser :: MonadBase (Brick.EventM ComponentName) m => BWL.List ComponentName Graphite.Metric -> MetricsBrowserWidget m
openMetricsBrowser browser = OpenMetricsBrowser {update, close, display}
  where
    width = fromMaybe 25 (maximumOf (traverse . to Graphite.metricLength) (browser ^. BWL.listElementsL))

    close = closedMetricsBrowser

    update = \(Modify keyPress) -> do
      adjustedList <- liftBase (BWL.handleListEventVi BWL.handleListEvent keyPress browser)
      return (openMetricsBrowser adjustedList)

    render isActive metric =
      let attrName = "metric" <> if isActive then "selected" else "unselcted"
       in Brick.withAttr attrName (Brick.txt (toText metric))

    display =
      let hasFocus = True
          popupSize = (width, 10)
       in Widget.centerLayer $
            WidgetB.border $
              Brick.setAvailableSize popupSize $
                BWL.renderList render hasFocus browser
