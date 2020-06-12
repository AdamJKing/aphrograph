{-# LANGUAGE TemplateHaskell #-}

module App.Components
  ( AppComponent (..),
    GraphDisplayWidget (..),
    GraphCanvasWidget (..),
    HorizontalAxisWidget (..),
    VerticalAxisWidget (..),
    BrickAppState (..),
    toDisplay,
  )
where

import qualified Brick.Types as Brick
import qualified Brick.Widgets.Border as Brick
import qualified Brick.Widgets.Center as Brick
import qualified Brick.Widgets.Core as Brick
import qualified Brick.Widgets.List as Brick
import Control.Lens.Getter
import Control.Lens.TH
import Data.Time.LocalTime
import Display.Graph
import qualified Graphite.Types as Graphite

data HorizontalAxisWidget = HorizontalAxis [Graphite.Time] TimeZone deriving (Show)

newtype VerticalAxisWidget = VerticalAxis [Graphite.Value] deriving (Show)

newtype GraphCanvasWidget = GraphCanvas (Graph Graphite.Time Graphite.Value) deriving (Show)

data GraphDisplayWidget = GraphDisplay GraphCanvasWidget VerticalAxisWidget HorizontalAxisWidget | LoadingDataDisplayWidget | NoDataDisplayWidget deriving (Show)

newtype ErrorWidget e = ErrorWidget e deriving (Show)

data AppComponent = GraphView | MetricsBrowserComponent
  deriving (Eq, Ord, Show)

newtype BrickAppState
  = BrickAppState
      { _metricsBrowser :: Brick.List AppComponent Graphite.Metric
      }

makeLenses ''BrickAppState

toDisplay :: BrickAppState -> Brick.Widget AppComponent
toDisplay appState =
  let metrics = appState ^. metricsBrowser
      hasFocus = True
      popupSize = (25, 10)
   in Brick.centerLayer $ Brick.border $ Brick.setAvailableSize popupSize $
        Brick.renderList renderListItem hasFocus metrics
  where
    renderListItem active (Graphite.Metric descriptor) =
      Brick.withAttr ("metric" <> if active then "selected" else "unselcted") (Brick.txt descriptor)
