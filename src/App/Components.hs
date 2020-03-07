module App.Components where

import qualified Brick.Widgets.List            as Brick
import qualified Graphite.Types                as Graphite
import           Data.Time.LocalTime
import           Display.Graph

data HorizontalAxisWidget = HorizontalAxis [Graphite.Time] TimeZone deriving Show
newtype VerticalAxisWidget = VerticalAxis [Graphite.Value] deriving Show
newtype GraphCanvasWidget = GraphCanvas (Graph Graphite.Time Graphite.Value) deriving Show

data GraphDisplayWidget = GraphDisplay GraphCanvasWidget VerticalAxisWidget HorizontalAxisWidget | NoDataDisplayWidget deriving Show

newtype ErrorWidget e = ErrorWidget e deriving Show

data AppComponent = GraphView | MetricsBrowserComponent
    deriving ( Eq, Ord, Show )

newtype MetricsBrowserWidget = MetricsBrowser (Brick.List AppComponent Graphite.Metric)

data AppWidget e = DefaultDisplay {
    dataDisplay :: !GraphDisplayWidget,
    metricBrowser :: Maybe MetricsBrowserWidget
  }

newtype DisplayWidget e = DisplayWidget ( Either ( ErrorWidget e ) ( AppWidget e ) )
