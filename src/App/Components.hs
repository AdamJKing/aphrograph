{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module App.Components
  ( AppWidget (..),
    DisplayWidget (..),
    ComponentName (..),
    MetricsBrowserWidget (..),
    ErrorWidget (..),
    GraphViewer (..),
    openMetricsBrowser,
    closedMetricsBrowser,
    targeted,
    MetricsBrowser,
  )
where

import qualified Brick as Brick
import qualified Brick.Widgets.Border as WidgetB
import qualified Brick.Widgets.Center as Widget
import qualified Brick.Widgets.List as BWL
import Control.Lens (maximumOf, to, (^.))
import Control.Monad.Base (MonadBase (liftBase))
import Data.Vector (Vector)
import Display.Graph (Graph)
import Display.GraphWidget (GraphWidget)
import Events.Types (MetricsBrowserEvent (Modify))
import qualified Graphite.Types as Graphite
import qualified Text.Show as Text

newtype ErrorWidget e = ErrorWidget e deriving (Show)

data ComponentName = GraphView | MetricsView
  deriving (Eq, Ord, Show)

type MetricsBrowser = BWL.List ComponentName Graphite.Metric

data MetricsBrowserWidget (m :: * -> *)
  = ClosedMetricsBrowser {open :: Vector Graphite.Metric -> MetricsBrowserWidget m}
  | OpenMetricsBrowser
      { update :: MetricsBrowserEvent -> m (MetricsBrowserWidget m),
        close :: MetricsBrowserWidget m,
        target :: () -> Maybe Graphite.Metric,
        display :: Brick.Widget ComponentName
      }
  deriving (Generic)

targeted :: MetricsBrowserWidget m -> Maybe Graphite.Metric
targeted (ClosedMetricsBrowser _) = Nothing
targeted (OpenMetricsBrowser {target}) = target ()

instance Text.Show (MetricsBrowserWidget m) where
  show (ClosedMetricsBrowser {}) = "ClosedMetricsBrowser"
  show (OpenMetricsBrowser {}) = "OpenMetricsBrowser"

class Monad m => GraphViewer m where
  updateGraph :: Graphite.GraphiteRequest -> Graph Graphite.Time Graphite.Value -> m GraphWidget
  default updateGraph ::
    (MonadTrans t, m ~ t n, GraphViewer n) =>
    Graphite.GraphiteRequest ->
    Graph Graphite.Time Graphite.Value ->
    m GraphWidget
  updateGraph req graph = lift (updateGraph req graph)

data AppWidget m = DefaultDisplay
  { dataDisplay :: GraphWidget,
    metricBrowser :: MetricsBrowserWidget m
  }

newtype DisplayWidget m e = DisplayWidget (Either (ErrorWidget e) (AppWidget m))

closedMetricsBrowser :: MonadBase (Brick.EventM ComponentName) m => MetricsBrowserWidget m
closedMetricsBrowser = ClosedMetricsBrowser {open}
  where
    open metrics = openMetricsBrowser (BWL.list MetricsView metrics 1)

openMetricsBrowser :: MonadBase (Brick.EventM ComponentName) m => BWL.List ComponentName Graphite.Metric -> MetricsBrowserWidget m
openMetricsBrowser browser = OpenMetricsBrowser {update, close, display, target}
  where
    width = fromMaybe 25 (maximumOf (traverse . to Graphite.metricLength) (browser ^. BWL.listElementsL))

    close = closedMetricsBrowser

    update = \(Modify keyPress) -> do
      adjustedList <- liftBase (BWL.handleListEventVi BWL.handleListEvent keyPress browser)
      return (openMetricsBrowser adjustedList)

    render isActive metric =
      let attrName = "metric" <> if isActive then "selected" else "unselcted"
       in Brick.withAttr attrName (Brick.txt (toText metric))

    target _ = snd <$> (BWL.listSelectedElement browser)

    display =
      let hasFocus = True
          popupSize = (width, 10)
       in Widget.centerLayer $
            WidgetB.border $
              Brick.setAvailableSize popupSize $
                BWL.renderList render hasFocus browser