{-# LANGUAGE FlexibleContexts #-}
module Display.Widgets where

import qualified Brick.Widgets.Core            as Brick
import           Fmt
import qualified Brick.Types                   as Brick
import           Display.Types
import           Control.Lens

import           Display.Graph
import           App
import           Graphics.Vty                  as Vty


verticalAxis :: AppState -> Brick.Widget AppComponent
verticalAxis appState = Brick.vLimit largestLabel $ Brick.Widget
    { hSize  = Brick.Fixed
    , vSize  = Brick.Greedy
    , render = return $ set Brick.imageL axis Brick.emptyResult
    }
  where
    labels = view (ui . displayLabelsY) appState
    height = uncurry subtract (boundsY (view (ui . displayData) appState))
    axis   = case organise height (vertical <$> labels) of
        Right full -> Vty.text mempty full
        Left  err  -> error ("Error occurred in vertical widget: " +|| err ||+ "")
    largestLabel = maximum (<$> labels)

horizontalAxis :: AppState -> Brick.Widget AppComponent
horizontalAxis appState = Brick.Widget { hSize  = Brick.Greedy
                                       , vSize  = Brick.Fixed
                                       , render = return $ set Brick.imageL axis Brick.emptyResult
                                       }
  where
    width = uncurry subtract (boundsX (view (ui . displayData) appState))
    axis  = case organise width (horizontal <$> view (ui . displayLabelsX) appState) of
        Right full -> Vty.text mempty full
        Left  err  -> error ("Error occurred in horizontal widget: " +|| err ||+ "")

graphWidget :: Graph Int Int -> Brick.Widget AppComponent
graphWidget graph = reportAndLimitSize $ Brick.Widget
    { hSize  = Brick.Greedy
    , vSize  = Brick.Greedy
    , render = return $ set Brick.imageL (toImage graph) Brick.emptyResult
    }
  where
    reportAndLimitSize =
        let width  = snd $ boundsX graph
            height = snd $ boundsY graph
        in  Brick.viewport GraphView Brick.Both
            . Brick.reportExtent GraphView
            . Brick.vLimit height
            . Brick.hLimit width
