{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Display.Graph.GraphBuilder where

import           Display.Graph
import           Display.Labels
import           Display.Types
import           Data.Array.ST
import           Graphite

newtype GraphWidgetState = GraphWidgetState {
    canvas :: Canvas
}

data GraphWidget x y = GraphWidget {
      xAxis :: [Label]
    , yAxis :: [Label]
    , graphData :: Graph x y
}

initialise :: (MArray (STUArray ()) Char m) => Dimensions Int -> m GraphWidgetState
initialise Dimensions {..} = GraphWidgetState <$> newArray ((0, 0), (width, height)) ' '

data UpdateEvent = GraphUpdate (Graph Value Time) | SizeUpdate (Dimensions Int)

class (Monad m) => Projector r r' m where
    project :: r -> m (Maybe r')
    original :: r' -> m (Maybe r)

data Projection r r' a = Projection r r' a

drawGraph
    :: (MonadFail m, MArray (STUArray ()) Char m, Projector (Value, Time) (Int, Int) m)
    => Graph Value Time
    -> Canvas
    -> m Canvas
drawGraph graph current =
    forM_ (assocs graph) (\point -> do
            ix <- project point
            case ix of
                Just i  -> writeArray current i '•'
                Nothing -> fail "Couldn't get index for range")
        >> return current

-- normaliseGraph :: (Integral n, Show n) => Graph Time Value -> Dimensions n -> Graph n n
-- normaliseGraph graph Dimensions {..} = mapPoints
--     (\(x, y) ->
--         let distributedX = getOrThrow $ scaleSpecial x
--             distributedY = getOrThrow $ scale y yBounds (0, height)
--         in  (distributedX, distributedY)
--     )
--     graph
--   where
--     xBounds = boundsX graph
--     yBounds = boundsY graph
--     scaleSpecial x = case scale x xBounds (0, width) of
--         Left BadOriginRange -> scale x (0, x + 10) (0, width)
--         other               -> other
--     getOrThrow = either (error . show) id

-- toImage :: (Ord x, Ord y, Num x, Num y, Enum x, Enum y) => Graph x y -> Vty.Image
-- toImage NoData = Vty.string mempty "No Data"
-- toImage graph  = Vty.vertCat [ mkRow y | y <- [yMin .. yMax] ]
--   where
--     mkRow y = Vty.horizCat [ renderChar x y | x <- [xMin .. xMax] ]
--     (xMin, xMax) = boundsX graph
--     (yMin, yMax) = boundsY graph
--     renderChar i j | (i, j) `member` graph = Vty.char mempty 'X'
--                    | otherwise             = Vty.char mempty ' '
