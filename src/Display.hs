{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Display where

import           Display.Graph.Widget
import           Display.Widgets
import qualified Brick.Widgets.Core            as Widget
import           Data.Vector.Lens               ( vector )
import           Control.Lens
import qualified App.State                     as App

newtype AppErrorWidget = AppErrorWidget App.Error deriving Show

instance CompileWidget n AppErrorWidget where
  compile (AppErrorWidget err) = Widget.str (show err)

data AppWidget = DefaultDisplay {
    dataDisplay :: !GraphDisplayWidget,
    metricBrowser :: Maybe MetricsBrowserWidget
  }

instance CompileLayeredWidget AppComponent AppWidget where
  compileLayered (DefaultDisplay dataDisplay Nothing        ) = [compile dataDisplay]
  compileLayered (DefaultDisplay dataDisplay (Just mBrowser)) = [compile mBrowser, compile dataDisplay]

newtype DisplayWidget = DisplayWidget ( Either AppErrorWidget AppWidget )

instance CompileLayeredWidget AppComponent DisplayWidget where
  compileLayered (DisplayWidget (Right appWidget  )) = compileLayered appWidget
  compileLayered (DisplayWidget (Left  errorWidget)) = return (compile errorWidget)

constructDom :: App.CurrentState -> DisplayWidget
constructDom (Left  (App.FailedState err)) = DisplayWidget $ Left (AppErrorWidget err)
constructDom (Right activeState          ) = DisplayWidget $ Right $ DefaultDisplay
  { dataDisplay   = graphDisplayWidget (activeState ^. App.graphData) (activeState ^. App.timezone)
  , metricBrowser = activeState ^? toMetricBrowser
  }
  where toMetricBrowser = App.metricsView . _Just . vector . to MetricsBrowser
