{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Display where

import           Display.Graph.Widget
import           Display.Widgets
import           App
import qualified Brick.Widgets.Core            as Widget
import           Data.Vector.Lens               ( vector )
import           Control.Lens

newtype AppErrorWidget = AppErrorWidget AppError deriving Show

data AppWidget = DefaultDisplay {
    dataDisplay :: GraphDisplayWidget,
    metricBrowser :: Maybe MetricsBrowserWidget
  } | ErrorDisplay AppErrorWidget

instance CompileWidget n AppErrorWidget where
  compile (AppErrorWidget err) = Widget.str (show err)

instance CompileLayeredWidget AppComponent AppWidget where
  compileLayered (ErrorDisplay errState                     ) = [compile errState]
  compileLayered (DefaultDisplay dataDisplay Nothing        ) = [compile dataDisplay]
  compileLayered (DefaultDisplay dataDisplay (Just mBrowser)) = [compile mBrowser, compile dataDisplay]

constructDom :: AppState -> AppWidget
constructDom (Failed err         ) = ErrorDisplay (AppErrorWidget err)
constructDom (Active currentState) = DefaultDisplay
  { dataDisplay   = graphDisplayWidget (currentState ^. graphData) (currentState ^. timezone)
  , metricBrowser = currentState ^? toMetricBrowser
  }
  where toMetricBrowser = metricsView . _Just . vector . to MetricsBrowser


