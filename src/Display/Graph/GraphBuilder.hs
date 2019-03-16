{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Display.Graph.GraphBuilder where

import           Display.Graph
import           Graphite
import           Display.Projection.Scalable
import           Brick.Types                   as Brick
import           Graphics.Vty                  as Vty
import           Data.Foldable                  ( maximum )
import           Control.Lens            hiding ( cons
                                                , snoc
                                                )
import           Display.Labels
import qualified Data.Map.Strict               as M
import           Prelude                 hiding ( (<|>) )
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as LT



graphWidget :: Graph Time Value -> Brick.Widget n
graphWidget graph = Widget
  { hSize  = Brick.Greedy
  , vSize  = Brick.Greedy
  , render = do
               image <- views heightAndWidthL (drawGraphImage graph)
               return (set imageL image emptyResult)
  }

verticalAxisWidget :: Graph Time Value -> Brick.Widget n
verticalAxisWidget graph = Widget
  { hSize  = Brick.Fixed
  , vSize  = Brick.Greedy
  , render = do
               image <- views
                 availHeightL
                 (\h -> drawVerticalAxisImage h (verticalAxis graph))
               return (set imageL image emptyResult)
  }

drawGraphImage :: Graph Time Value -> (Int, Int) -> Vty.Image
drawGraphImage NoData _ = Vty.text mempty "No Data"
drawGraphImage graph (width, height) =
  foldl' appendNextColumn Vty.emptyImage
    $! [ M.findWithDefault 0 i (normaliseGraph $ toMap graph)
       | i <- [0 .. width]
       ]
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

drawVerticalAxisImage :: Int -> [Value] -> Vty.Image
drawVerticalAxisImage height values =
  let labels = M.fromAscList $ generateLabelsContinuous values (0, height)
      rows   = (`M.lookup` labels) <$> [0..height]
  in  vertCat $ buildRow (largest labels) <$> reverse rows
 where
  largest = (+ 1) . maximum . fmap T.length
  buildRow width = maybe (drawDefaultLine width) (drawLabelledLine width)


drawLabelledLine :: Int -> Text -> Vty.Image
drawLabelledLine (fromIntegral -> w) =
  Vty.text mempty . LT.justifyRight w ' ' . (`LT.snoc` '\9508') . fromStrict

drawDefaultLine :: Int -> Vty.Image
drawDefaultLine (fromIntegral -> w) =
  Vty.text mempty $ LT.justifyRight (fromIntegral w) ' ' "\9474"
