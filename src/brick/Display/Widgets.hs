{-# LANGUAGE FlexibleContexts #-}
module Display.Widgets where

import           Display
import qualified Brick.Widgets.Core            as Brick
import           Brick.Types                   as Brick
import           Display.Types
import           Control.Lens

import           Display.Axis
import           Display.Graph
import           App
import           Data.Hourglass


horizontalAxisWidget :: AppState -> Brick.Widget AppComponent
horizontalAxisWidget AppState { appData = graph } =
    Brick.vLimit graphHeight . Brick.hLimit graphWidth $ Brick.Widget
        { hSize  = Greedy
        , vSize  = Fixed
        , render = do
                       ctxt <- getContext
                       let width = view availWidthL ctxt
                       return $ set
                           imageL
                           (toHorizontalAxis width $ mapX intFromElapsed graph)
                           emptyResult
        }
  where
    graphWidth     = intFromElapsed . snd $ boundsX graph
    graphHeight    = round . snd $ boundsY graph
    intFromElapsed = fromIntegral . toSeconds . timeGetNanoSeconds

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
