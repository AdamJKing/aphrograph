{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Display.GraphWidget
  ( GraphWidget (..),
    GraphCanvasWidget(..),
    GraphDisplay (..),
    HorizontalAxisWidget (..),
    VerticalAxisWidget (..),
    graphDisplayWidget,
    drawGraphImage,
    drawVerticalAxisImage,
    drawHorizontalAxisImage,
    cornerPiece,
    heightAndWidthL,
    graphiteRequest,
    graphDisplay,
  )
where

import Brick.Types as Brick
  ( Context,
    Padding (Max),
    Widget,
    availHeightL,
    availWidthL,
  )
import Brick.Widgets.Core as W (padBottom, padLeft, txt)
import Control.Lens
  ( Getter,
    ReifiedGetter (Getter, runGetter),
    maximumOf,
    to,
  )
import Control.Lens.TH (makeLenses)
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import qualified Data.Text.Lazy as LT
import Data.Time.LocalTime (TimeZone)
import Display.Graph as G
  ( Graph,
    boundsX,
    boundsY,
    horizontalAxis,
    null,
    toMap,
    verticalAxis,
  )
import Display.Labels
  ( generateLabelsContinuous,
    generateLabelsTime,
  )
import Display.Projection.Scalable (Scalable (scale))
import Graphics.Vty
  ( horizJoin,
    vertCat,
    vertJoin,
  )
import qualified Graphics.Vty as Vty
import Graphite.Types as Graphite (GraphiteRequest, Time, Value)

data HorizontalAxisWidget = HorizontalAxis [Graphite.Time] TimeZone deriving (Show, Generic)

newtype VerticalAxisWidget = VerticalAxis [Graphite.Value] deriving (Show, Generic)

newtype GraphCanvasWidget = GraphCanvas (Graph Graphite.Time Graphite.Value) deriving (Show, Generic)

data GraphDisplay
  = GraphDisplay
      { displayCanvas :: GraphCanvasWidget,
        vertAxis :: VerticalAxisWidget,
        horizAxis :: HorizontalAxisWidget
      }
  | LoadingDataDisplay
  | NoDataDisplay
  deriving (Show, Generic)

data GraphWidget = GraphWidget
  { _graphiteRequest :: Graphite.GraphiteRequest,
    _graphDisplay :: GraphDisplay
  }
  deriving (Generic, Show)

makeLenses ''GraphWidget

graphDisplayWidget :: GraphiteRequest -> Graph Time Value -> TimeZone -> GraphWidget
graphDisplayWidget _graphiteRequest graph _timezone =
  GraphWidget {_graphiteRequest, _graphDisplay}
  where
    _graphDisplay
      | G.null graph = NoDataDisplay
      | otherwise =
        GraphDisplay
          (GraphCanvas graph)
          (VerticalAxis (verticalAxis graph))
          (HorizontalAxis (horizontalAxis graph) _timezone)

drawGraphImage :: Graph Time Value -> (Int, Int) -> Vty.Image
drawGraphImage graph (width, height) =
  if G.null graph
    then Vty.text mempty "No Data"
    else
      foldl' appendNextColumn Vty.emptyImage
        $! [M.findWithDefault 0 i (normaliseGraph $ toMap Set.findMin graph) | i <- [0 .. width]]
  where
    graphX = boundsX graph
    graphY = boundsY graph
    diff (a, b) = b - a
    expandIfNeeded (a, b) = if diff (a, b) == 0 then (a - 10, b + 10) else (a, b)
    appendNextColumn = (. buildColumn height) . horizJoin
    normaliseGraph =
      M.map (\v -> scale v (expandIfNeeded graphY) (0, height))
        . M.mapKeysWith (\old new -> (old + new) / 2) (\k -> scale k (expandIfNeeded graphX) (0, width))

drawVerticalAxisImage :: Int -> [Value] -> Vty.Image
drawVerticalAxisImage height values =
  let labels = M.fromAscList $ generateLabelsContinuous values (0, height)
      width = fromIntegral $ largest labels
      rows = (`M.lookup` labels) <$> [height, (height -1) .. 0]
      buildRow = maybe (drawDefaultLine width) (drawLabelledLine width)
   in vertCat $ buildRow <$> rows
  where
    largest = maybe 5 (+ 1) . maximumOf (traverse . to LT.length)

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
buildColumn height value = vertCat $ [drawPixelAt i | i <- [height, height -1 .. 0]]
  where
    drawPixelAt i = Vty.char mempty (if i == value then 'X' else ' ')

heightAndWidthL :: Getter Brick.Context (Int, Int)
heightAndWidthL = runGetter $ (,) <$> Getter availWidthL <*> Getter availHeightL

drawLabelledLine :: Int -> LText -> Vty.Image
drawLabelledLine (fromIntegral -> w) = Vty.text mempty . prependSpace w . (`LT.snoc` '\9508')

drawDefaultLine :: Int -> Vty.Image
drawDefaultLine (fromIntegral -> w) = Vty.text mempty $ prependSpace w "\9474"

drawLabelledBlock :: Int -> Int -> LText -> Vty.Image
drawLabelledBlock offset current label =
  let width = fromIntegral $ max (current - offset) 0
      topBar = Vty.text mempty $ LT.replicate (width - 1) "\9472" `LT.snoc` '\9516'
      labelBar = Vty.text mempty $ prependSpace width $ LT.take (width - 1) label
   in topBar `vertJoin` labelBar

prependSpace :: Int64 -> LText -> LText
prependSpace w = LT.justifyRight w ' '
