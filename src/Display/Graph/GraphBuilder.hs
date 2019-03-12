{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Display.Graph.GraphBuilder where

import           Display.Graph
import           Display.Canvas                as Canvas
import           Display.Labels
import           Display.Types
import           Graphite
import           Display.Projection.Scalable

data GraphWidgetState s = GraphWidgetState {
    graphCanvas :: TVar ( Canvas s ),
    xAxisCanvas :: TVar (Canvas s),
    yAxisCanvas :: TVar (Canvas s)
}

data GraphWidget x y = GraphWidget {
      xAxis :: [Label]
    , yAxis :: [Label]
    , graphData :: Graph x y
}

data UpdateEvent = GraphUpdate (Graph Value Time) | SizeUpdate (Dimensions Int)

drawGraph :: Graph Value Time -> CanvasUpdate s ()
drawGraph graph = forM_ (assocs graph) $ \(x, y) -> do
  canvasX <- Canvas.xBounds
  let x' = scale x graphX canvasX

  canvasY <- Canvas.yBounds
  let y' = scale y graphY canvasY

  paintPoint (x', y')

 where
  graphX = boundsX graph
  graphY = boundsY graph

