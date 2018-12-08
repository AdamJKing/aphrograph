{-# LANGUAGE FlexibleContexts #-}
module Display.Widgets where

import           Display
import qualified Brick.Widgets.Core            as Brick
import           Brick.Types                   as Brick
import           Display.Graph
import           Control.Lens
import           App

verticalAxisWidget :: Graph Integer Integer -> Brick.Widget Components
verticalAxisWidget graph = Brick.Widget
    { hSize  = Greedy
    , vSize  = Fixed
    , render = return $ set imageL (toVerticalAxis graph) emptyResult
    }

graphWidget :: Graph Integer Integer -> Brick.Widget Components
graphWidget graph =
    Brick.viewport GraphView Both
        . Brick.reportExtent GraphView
        . Brick.vLimit (fromInteger height)
        . Brick.hLimit (fromInteger width)
        $ Brick.Widget
              { hSize  = Greedy
              , vSize  = Greedy
              , render = return $ set imageL (toImage graph) emptyResult
              }
    where (width, height) = (snd $ boundsX graph, snd $ boundsY graph)
