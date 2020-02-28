{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Display.GraphWidget where

import App.Components
import Brick.Types as Brick
import Brick.Widgets.Core as W
import Control.Lens hiding
  ( cons,
    snoc,
  )
import Data.Foldable (maximum)
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Time.LocalTime
import Display.Graph as G
import Display.Labels
import Display.Projection.Scalable
import qualified Graphics.Vty as Vty
import Graphics.Vty
  ( horizJoin,
    vertCat,
    vertJoin,
  )
import Graphite.Types
import Prelude hiding ((<|>))

graphDisplayWidget :: Graph Time Value -> TimeZone -> GraphDisplayWidget
graphDisplayWidget graph timezone =
  if G.null graph
    then NoDataDisplayWidget
    else
      GraphDisplay
        (GraphCanvas graph)
        (VerticalAxis (verticalAxis graph))
        (HorizontalAxis (horizontalAxis graph) timezone)

normaliseGraph :: (Fractional y, Real y, Scalable x, Scalable i) => (x, x) -> (y, y) -> (i, i) -> Map x y -> Map i i
normaliseGraph xBounds yBounds (width, height) =
  M.map (\v -> scale v (expandIfNeeded yBounds) (0, height))
    . M.mapKeysWith (\old new -> (old + new) / 2) (\k -> scale k (expandIfNeeded xBounds) (0, width))
  where
    expandIfNeeded (lower, higher) = if higher - lower == 0 then (lower - 10, higher + 10) else (lower, higher)

drawGraphImage :: Graph Time Value -> (Int, Int) -> Vty.Image
drawGraphImage graph (width, height) =
  if G.null graph
    then Vty.text mempty "No Data"
    else
      foldl' appendNextColumn Vty.emptyImage
        $! [M.findWithDefault 0 i (normaliseGraph graphX graphY (width, height) $ toMap Set.findMin graph) | i <- [0 .. width]]
  where
    graphX = boundsX graph
    graphY = boundsY graph
    appendNextColumn = (. buildColumn height) . horizJoin

drawVerticalAxisImage :: Int -> [Value] -> Vty.Image
drawVerticalAxisImage height values =
  let labels = M.fromAscList $ generateLabelsContinuous values (0, height)
      rows = (`M.lookup` labels) <$> [0 .. height]
   in vertCat $ buildRow (largest labels) <$> reverse rows
  where
    largest = (+ 1) . maximum . fmap T.length
    buildRow width = maybe (drawDefaultLine width) (drawLabelledLine width)

drawHorizontalAxisImage :: TimeZone -> Int -> [Time] -> Vty.Image
drawHorizontalAxisImage tz width values =
  let labels = generateLabelsTime tz values (0, width)
      axis = buildImage labels $ \prev (pos, label) -> prev `horizJoin` buildNextBlock prev pos label
   in axis `horizJoin` makeFill (width - Vty.imageWidth axis)
  where
    buildNextBlock = drawLabelledBlock . Vty.imageWidth
    buildImage labels f = foldl' f Vty.emptyImage labels
    makeFill w = Vty.charFill mempty '\9472' w 1

cornerPiece :: Widget n
cornerPiece = padBottom Max $ padLeft Max $ txt "\9492"

buildColumn :: Int -> Int -> Vty.Image
buildColumn height value = vertCat $! reverse [drawPixelAt i | i <- [0 .. height]]
  where
    drawPixelAt i = Vty.char mempty (if i == value then 'X' else ' ')

heightAndWidthL :: Getter Brick.Context (Int, Int)
heightAndWidthL = runGetter $ (,) <$> Getter availWidthL <*> Getter availHeightL

drawLabelledLine :: Int -> Text -> Vty.Image
drawLabelledLine (fromIntegral -> w) = Vty.text mempty . prependSpace w . (`LT.snoc` '\9508') . fromStrict

drawDefaultLine :: Int -> Vty.Image
drawDefaultLine (fromIntegral -> w) = Vty.text mempty $ prependSpace w "\9474"

drawLabelledBlock :: Int -> Int -> Text -> Vty.Image
drawLabelledBlock offset current label =
  let width = fromIntegral $ max (current - offset) 0
      topBar = Vty.text mempty $ LT.replicate (width - 1) "\9472" `LT.snoc` '\9516'
      labelBar = Vty.text mempty $ prependSpace width $ LT.take (width - 1) (fromStrict label)
   in topBar `vertJoin` labelBar

prependSpace :: Int64 -> LText -> LText
prependSpace w = LT.justifyRight w ' '
