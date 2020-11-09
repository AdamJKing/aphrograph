{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Display.Widgets where

import App.Components
  ( ComponentName (GraphView),
    MetricsBrowser (..),
    TimeDialogue (OpenDialogue),
  )
import qualified App.State as App
import qualified Brick
import Brick.Types as Brick
  ( RenderM,
    Result,
    Size (Greedy),
    Widget (..),
    emptyResult,
    imageL,
  )
import Brick.Widgets.Border as WidgetB (border)
import Brick.Widgets.Center as Widget (center, centerLayer)
import Brick.Widgets.Core as Widget
  ( cached,
    emptyWidget,
    hBox,
    hLimitPercent,
    padAll,
    str,
    vBox,
    vLimitPercent,
  )
import qualified Brick.Widgets.Dialog as Widget
import qualified Brick.Widgets.List as Brick
import Control.Lens (set, view, views)
import Display.GraphWidget
  ( GraphCanvasWidget (..),
    GraphDisplay (..),
    GraphWidget (..),
    HorizontalAxisWidget (..),
    VerticalAxisWidget (..),
    cornerPiece,
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

instance CompileWidget ComponentName MetricsBrowser where
  compile (MkMetricsBrowser browser width) =
    let hasFocus = True
        popupSize = (width, 10)
     in Widget.centerLayer $
          WidgetB.border $
            Brick.setAvailableSize popupSize $
              Brick.renderList render hasFocus browser
    where
      render isActive metric =
        let attrName = "metric" <> if isActive then "selected" else "unselcted"
         in Brick.withAttr attrName (Brick.txt (toText metric))

instance CompileWidget ComponentName TimeDialogue where
  compile (OpenDialogue dialogue) = Widget.renderDialog dialogue Widget.emptyWidget

instance CompileLayeredWidget ComponentName App.CurrentState where
  compileLayered (App.Failed (App.FailedState err)) = [Widget.str (displayException err)]
  compileLayered (App.Active (App.ActiveState {..})) = case _dialogue of
    Just (Right td) -> [compile td, compile _graphData]
    Just (Left mv) -> [compile mv, compile _graphData]
    Nothing -> [compile _graphData]

instance CompileWidget ComponentName GraphWidget where
  compile GraphWidget {_graphDisplay = NoDataDisplay} = Widget.str "NoData"
  compile GraphWidget {_graphDisplay = LoadingDataDisplay} = Widget.center $ WidgetB.border $ Widget.padAll 1 $ Widget.str "Loading..."
  compile GraphWidget {_graphDisplay = (GraphDisplay graphCanvas verticalAxis' horizontalAxis')} =
    cached GraphView $ arrange (compile graphCanvas) (compile verticalAxis') (compile horizontalAxis')
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
