{-# LANGUAGE FlexibleContexts #-}
module Display.Widgets where

import           Display
import qualified Brick.Widgets.Core            as Brick
import           Fmt
import           Brick.Types                   as Brick
import           Display.Types
import           Control.Lens

import           Display.Graph
import           App
import           Labels
import           Graphics.Vty                  as Vty


horizontalAxisWidget :: AppState -> Brick.Widget AppComponent
horizontalAxisWidget AppState { _ui = UI { _displayData = displayGraph, _displayLabels = labels } }
    = Brick.Widget { hSize = Greedy, vSize = Fixed, render = return $ set imageL axis emptyResult }
  where
    width = case boundsX displayGraph of
        (a, b) -> b - a
    axis = case organiseLabels width labels of
        Right full -> Vty.string mempty $ toString full
        Left  err  -> error ("Error occurred in horizontal widget: " +|| err ||+ "")

graphWidget :: Graph Int Int -> Brick.Widget AppComponent
graphWidget graph = reportAndLimitSize $ Brick.Widget
    { hSize  = Greedy
    , vSize  = Greedy
    , render = return $ set imageL (toImage graph) emptyResult
    }
  where
    reportAndLimitSize =
        let width  = snd $ boundsX graph
            height = snd $ boundsY graph
        in  Brick.viewport GraphView Both
            . Brick.reportExtent GraphView
            . Brick.vLimit height
            . Brick.hLimit width
