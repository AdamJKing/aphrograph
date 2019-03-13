{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Display.Graph.GraphBuilder where

import           Display.Graph
import           Graphite
import           Display.Projection.Scalable
import           Brick.Types                   as Brick
import           Graphics.Vty                  as Vty
import           Control.Lens
import qualified Data.Map.Strict               as M
import           Prelude                 hiding ( (<|>) )


graphWidget :: Graph Time Value -> Brick.Widget n
graphWidget graph = Widget
  { hSize  = Brick.Greedy
  , vSize  = Brick.Greedy
  , render = do
               image <- views heightAndWidthL (drawGraphImage graph)
               return (set imageL image emptyResult)
  }

drawGraphImage :: Graph Time Value -> (Int, Int) -> Vty.Image
drawGraphImage NoData _               = Vty.text mempty "No Data"
drawGraphImage graph  (width, height) = foldl' appendNextColumn Vty.emptyImage
  $! [ M.findWithDefault 0 i (normaliseGraph $ toMap graph) | i <- [0..width]]
 where
  graphX = boundsX graph
  graphY = boundsY graph
  diff (a, b) = b - a
  expandIfNeeded (a, b) = if diff (a, b) == 0 then (a - 10, b + 10) else (a, b)
  appendNextColumn = (. buildColumn height) . horizJoin
  normaliseGraph =
    M.map (\v -> scale v (expandIfNeeded graphY) (0, height)) . M.mapKeysWith
      (\old new -> (old + new) / 2)
      (\k -> scale k (expandIfNeeded graphX) (0, width))

buildColumn :: Int -> Int -> Vty.Image
buildColumn height value = vertCat
  $! reverse [ drawPixelAt i | i <- [0 .. height] ]
  where drawPixelAt i = Vty.char mempty (if i == value then 'X' else ' ')

heightAndWidthL :: Getter Brick.Context (Int, Int)
heightAndWidthL =
  runGetter $ (,) <$> Getter availWidthL <*> Getter availHeightL
