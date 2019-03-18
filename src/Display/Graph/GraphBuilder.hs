{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Display.Graph.GraphBuilder where

import           Display.Graph
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
import           Graphite.Types


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
  { hSize  = Brick.Greedy
  , vSize  = Brick.Greedy
  , render = do
               image <- views heightAndWidthL $ \(w, h) ->
                 let img = drawVerticalAxisImage h (verticalAxis graph)
                 in  Vty.pad (w - Vty.imageWidth img) 0 0 0 img
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
      rows   = (`M.lookup` labels) <$> [0 .. height]
  in  vertCat $ buildRow (largest labels) <$> reverse rows
 where
  largest = (+ 1) . maximum . fmap T.length
  buildRow width = maybe (drawDefaultLine width) (drawLabelledLine width)

drawLabelledLine :: Int -> Text -> Vty.Image
drawLabelledLine (fromIntegral -> w) =
  Vty.text mempty . prependSpace w . (`LT.snoc` '\9508') . fromStrict

drawDefaultLine :: Int -> Vty.Image
drawDefaultLine (fromIntegral -> w) = Vty.text mempty $ prependSpace w "\9474"

horizontalAxisWidget :: Graph Time y -> Widget n
horizontalAxisWidget graph = Widget
  { hSize  = Brick.Greedy
  , vSize  = Brick.Greedy
  , render = do
               image <- views heightAndWidthL $ \(w, h) ->
                 let img = drawHorizontalAxisImage w (horizontalAxis graph)
                 in  Vty.pad 0 0 0 (h - Vty.imageHeight img) img
               return (set imageL image emptyResult)
  }

drawHorizontalAxisImage :: Int -> [Time] -> Vty.Image
drawHorizontalAxisImage width values =
  let labels = tail . fromList $ generateLabelsDiscrete values (0, width)
      axis   = buildImage labels
        $ \prev (pos, label) -> prev `horizJoin` buildNextBlock prev pos label
  in  axis `horizJoin` makeFill (width - Vty.imageWidth axis)
 where
  buildNextBlock = drawLabelledBlock . Vty.imageWidth
  buildImage labels f = foldl' f Vty.emptyImage labels
  makeFill w = Vty.charFill mempty '\9472' w 1

drawLabelledBlock :: Int -> Int -> Text -> Vty.Image
drawLabelledBlock offset current label =
  let
    width = fromIntegral $ max (current - offset) 0
    topBar =
      Vty.text mempty $ LT.replicate (width - 1) "\9472" `LT.snoc` '\9516'
    labelBar = Vty.text mempty $ prependSpace width $ LT.take
      (width - 1)
      (fromStrict label)
  in
    topBar `vertJoin` labelBar

drawDefaultColumn :: Int -> Vty.Image
drawDefaultColumn (fromIntegral -> w) =
  Vty.text mempty $ prependSpace w "\9474"

prependSpace :: Int64 -> LText -> LText
prependSpace w = LT.justifyRight w ' '
