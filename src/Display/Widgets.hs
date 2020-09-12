{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Display.Widgets where

import App.Components
  ( AppWidget (DefaultDisplay),
    ComponentName (GraphView),
    DisplayWidget (..),
    ErrorWidget (..),
    GraphCanvasWidget (..),
    GraphDisplayWidget (..),
    HorizontalAxisWidget (..),
    MetricsBrowserWidget (display),
    VerticalAxisWidget (..),
  )
import Brick.Types as Brick
  ( RenderM,
    Result,
    Size (Greedy),
    Widget (..),
    emptyResult,
    imageL,
  )
import Brick.Widgets.Border as WidgetB (border)
import Brick.Widgets.Center as Widget (center)
import Brick.Widgets.Core as Widget
  ( cached,
    hBox,
    hLimitPercent,
    padAll,
    str,
    vBox,
    vLimitPercent,
  )
import Control.Lens (set, view, views)
import Display.GraphWidget
  ( cornerPiece,
    drawGraphImage,
    drawHorizontalAxisImage,
    drawVerticalAxisImage,
    heightAndWidthL,
  )
import qualified Graphics.Vty as Vty

class CompileWidget n w where
  compile :: w -> Brick.Widget n

class CompileLayeredWidget n w where
  compileLayered :: w -> [Brick.Widget n]

instance CompileWidget ComponentName (MetricsBrowserWidget m) where
  compile = display

instance CompileLayeredWidget ComponentName (AppWidget e) where
  compileLayered (DefaultDisplay dataDisplay mBrowser) = [compile mBrowser, compile dataDisplay]

instance Exception e => CompileLayeredWidget ComponentName (DisplayWidget m e) where
  compileLayered (DisplayWidget (Right appWidget)) = compileLayered appWidget
  compileLayered (DisplayWidget (Left errorWidget)) = return (compile errorWidget)

instance Exception e => CompileWidget n (ErrorWidget e) where
  compile (ErrorWidget err) = Widget.str (displayException err)

instance CompileWidget ComponentName GraphDisplayWidget where
  compile NoDataDisplayWidget = Widget.str "NoData"
  compile LoadingDataDisplayWidget = Widget.center $ WidgetB.border $ Widget.padAll 1 $ Widget.str "Loading..."
  compile (GraphDisplay graphCanvas verticalAxis' horizontalAxis') =
    let graphWidget = compile graphCanvas
        horizontalAxisWidget = compile horizontalAxis'
        verticalAxisWidget = compile verticalAxis'
     in cached GraphView $ arrange graphWidget verticalAxisWidget horizontalAxisWidget
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
