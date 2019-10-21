{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Display where

import           Display.Graph.Widget
import           Display.Widgets
import qualified Brick.Widgets.Core            as Widget
import           Data.Vector.Lens               ( vector )
import           Control.Lens
import           App.State                     as App

newtype AppErrorWidget = AppErrorWidget App.Error deriving Show

data AppWidget = DefaultDisplay {
    dataDisplay :: GraphDisplayWidget,
    metricBrowser :: Maybe MetricsBrowserWidget
  }

instance CompileWidget n AppErrorWidget where
  compile (AppErrorWidget err) = Widget.str (show err)

instance CompileLayeredWidget AppComponent AppWidget where
  compileLayered (DefaultDisplay dataDisplay Nothing        ) = [compile dataDisplay]
  compileLayered (DefaultDisplay dataDisplay (Just mBrowser)) = [compile mBrowser, compile dataDisplay]

constructDom :: ActiveState -> AppWidget
constructDom currentState = DefaultDisplay
  { dataDisplay   = graphDisplayWidget (currentState ^. graphData) (currentState ^. timezone)
  , metricBrowser = currentState ^? toMetricBrowser
  }
  where toMetricBrowser = metricsView . _Just . vector . to MetricsBrowser
