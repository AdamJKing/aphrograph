{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Display.Widgets where

import App.Components
import Brick.Types as Brick
import Brick.Widgets.Border as WidgetB
import Brick.Widgets.Center as Widget
import Brick.Widgets.Core as Widget
import Brick.Widgets.List as Widget
import Control.Lens.Getter
import Control.Lens.Setter
import Display.GraphWidget
import qualified Graphics.Vty as Vty
import Graphite.Types

class CompileWidget n w where
  compile :: w -> Brick.Widget n

class CompileLayeredWidget n w where
  compileLayered :: w -> [Brick.Widget n]

instance CompileWidget AppComponent MetricsBrowserWidget where
  compile (MetricsBrowser metricsList) =
    let hasFocus = True
        popupSize = (25, 10)
     in Widget.centerLayer $ WidgetB.border $ Widget.setAvailableSize popupSize $
          Widget.renderList
            ( \active (Metric descriptor) ->
                Widget.withAttr ("metric" <> if active then "selected" else "unselcted") (Widget.txt descriptor)
            )
            hasFocus
            metricsList

instance CompileLayeredWidget AppComponent AppWidget where
  compileLayered (DefaultDisplay dataDisplay Nothing) = [compile dataDisplay]
  compileLayered (DefaultDisplay dataDisplay (Just mBrowser)) = [compile mBrowser, compile dataDisplay]

instance Exception e => CompileLayeredWidget AppComponent (DisplayWidget e) where
  compileLayered (DisplayWidget (Right appWidget)) = compileLayered appWidget
  compileLayered (DisplayWidget (Left errorWidget)) = return (compile errorWidget)

instance Exception e => CompileWidget n (ErrorWidget e) where
  compile (ErrorWidget err) = Widget.str (displayException err)

instance CompileWidget n GraphDisplayWidget where
  compile NoDataDisplayWidget = Widget.str "NoData"
  compile (GraphDisplay graphCanvas verticalAxis' horizontalAxis') =
    let graphWidget = compile graphCanvas
        horizontalAxisWidget = compile horizontalAxis'
        verticalAxisWidget = compile verticalAxis'
     in arrange graphWidget verticalAxisWidget horizontalAxisWidget
    where
      arrange g v h =
        Widget.vBox
          [ Widget.vLimitPercent 90 $ Widget.hBox [Widget.hLimitPercent 8 v, g],
            Widget.hBox [Widget.hLimitPercent 8 cornerPiece, h]
          ]

instance CompileWidget n GraphCanvasWidget where
  compile (GraphCanvas canvasData) =
    Widget
      { hSize = Brick.Greedy,
        vSize = Brick.Greedy,
        render = do
          image <- views heightAndWidthL (drawGraphImage canvasData)
          return (set imageL image emptyResult)
      }

instance CompileWidget n VerticalAxisWidget where
  compile (VerticalAxis values) = Widget {hSize = Brick.Greedy, vSize = Brick.Greedy, render = renderImg}
    where
      expandToFit w img = Vty.pad (w - Vty.imageWidth img) 0 0 0 img
      createImg (w, h) = expandToFit w $ drawVerticalAxisImage h values
      renderImg = do
        image <- views heightAndWidthL createImg
        return (set imageL image emptyResult)

instance CompileWidget n HorizontalAxisWidget where
  compile (HorizontalAxis values tz) = Widget {hSize = Brick.Greedy, vSize = Brick.Greedy, render = renderImg}
    where
      renderImg :: RenderM n (Result n)
      renderImg = do
        (width, height) <- view heightAndWidthL
        let img = drawHorizontalAxisImage tz width values
        let paddedImg = Vty.pad 0 0 0 (height - Vty.imageHeight img) img
        return (set imageL paddedImg emptyResult)
