{-# LANGUAGE FlexibleContexts #-}
module Display.Widgets where

import           Display
import qualified Brick.Widgets.Core            as Brick
import           Brick.Types                   as Brick
import           Display.Graph
import           Control.Lens
import           App

graphWidget :: Graph Integer Integer -> Brick.Widget Components
graphWidget graph =
    Brick.viewport GraphView Both
        . Brick.reportExtent GraphView
        . Brick.vLimit (fromInteger height)
        . Brick.hLimit (fromInteger width)
        $ Brick.Widget
              { hSize  = Fixed
              , vSize  = Fixed
              , render = return $ set imageL (toImage graph) emptyResult
              }
    where (width, height) = (snd $ boundsX graph, snd $ boundsY graph)
