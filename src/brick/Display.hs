{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Display where

import qualified Graphics.Vty                  as Vty
import           Normalisation
import           Display.Graph
import           Display.Types
import           Data.Hourglass


normaliseGraph
    :: (Integral n, Show n) => Graph Elapsed Double -> Dimensions n -> Graph n n
normaliseGraph graph Dimensions {..} = mapPoints
    (\(x, y) ->
        let distributedX = getOrThrow $ scaleSpecial x
            distributedY = getOrThrow $ scale y yBounds (0, height)
        in  (distributedX, distributedY)
    )
    graph
  where
    xBounds = boundsX graph
    yBounds = boundsY graph
    scaleSpecial x = case scale x xBounds (0, width) of
        Left BadOriginRange -> scale x (0, x + 10) (0, width)
        other               -> other
    getOrThrow = either (error . show) id

toImage
    :: (Ord x, Ord y, Num x, Num y, Enum x, Enum y) => Graph x y -> Vty.Image
toImage NoData = Vty.string mempty "No Data"
toImage graph  = Vty.vertCat [ mkRow y | y <- [yMin .. yMax] ]
  where
    mkRow y = Vty.horizCat [ renderChar x y | x <- [xMin .. xMax] ]
    (xMin, xMax) = boundsX graph
    (yMin, yMax) = boundsY graph
    renderChar i j | (i, j) `member` graph = Vty.char mempty 'X'
                   | otherwise             = Vty.char mempty ' '
