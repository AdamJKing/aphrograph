{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Display where

import qualified Graphics.Vty                  as Vty
import           Normalisation
import           Display.Graph
import           Display.Types
import           Data.Hourglass
import           Control.Monad.Log
import           Data.Text.Prettyprint.Doc
                                         hiding ( width )

normaliseGraph
    :: (MonadLog (Doc String) m)
    => Graph Seconds Double
    -> Dimensions Integer
    -> m (Graph Integer Integer)
normaliseGraph graph Dimensions {..} = do
    g <- mapPointsM
        (\(x, y) -> do
            let distributedX = getOrThrow $ scaleSpecial x
            let distributedY = getOrThrow $ scale y yBounds (0, height)
            logMessage . pretty $ show (distributedX, distributedY)
            return (distributedX, distributedY)
        )
        graph
    logMessage . pretty $ show g
    return g
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
