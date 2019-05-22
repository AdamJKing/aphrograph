{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Display.Graph.GraphBuilder where

import           Display.Graph
import           Display.Projection.Scalable
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
import           Brick.Types                   as Brick
import           Brick.Widgets.Core            as Brick
import           App
import           Data.Time.LocalTime

type GraphBuilder n = Reader AppState (Brick.Widget n)

build :: GraphBuilder n -> AppState -> Brick.Widget n
build = runReader

cornerPiece :: Widget n
cornerPiece = padBottom Max $ padLeft Max $ txt "\9492"

graphDisplayWidget :: GraphBuilder n
graphDisplayWidget =
    arrange <$> graphWidget <*> verticalAxisWidget <*> horizontalAxisWidget
  where
    arrange g v h = Brick.vBox
        [ Brick.vLimitPercent 90 $ Brick.hBox [Brick.hLimitPercent 8 v, g]
        , Brick.hBox [Brick.hLimitPercent 8 cornerPiece, h]
        ]

graphWidget :: GraphBuilder n
graphWidget = asks (buildWidget . graphData)
  where
    buildWidget Nothing      = txt "Couldn't get app state."
    buildWidget (Just graph) = Widget { hSize  = Brick.Greedy
                                      , vSize  = Brick.Greedy
                                      , render = renderImg
                                      }
      where
        renderImg = do
            image <- views heightAndWidthL (drawGraphImage graph)
            return (set imageL image emptyResult)

verticalAxisWidget :: GraphBuilder n
verticalAxisWidget = asks (buildWidget . graphData)
  where
    buildWidget Nothing      = Brick.emptyWidget
    buildWidget (Just graph) = Widget { hSize  = Brick.Greedy
                                      , vSize  = Brick.Greedy
                                      , render = renderImg
                                      }
      where
        expandToFit w img = Vty.pad (w - Vty.imageWidth img) 0 0 0 img

        createImg g (w, h) =
            expandToFit w $ drawVerticalAxisImage h $ verticalAxis g

        renderImg = do
            image <- views heightAndWidthL $ createImg graph
            return (set imageL image emptyResult)

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

    expandIfNeeded (a, b) =
        if diff (a, b) == 0 then (a - 10, b + 10) else (a, b)

    appendNextColumn = (. buildColumn height) . horizJoin

    normaliseGraph =
        M.map (\v -> scale v (expandIfNeeded graphY) (0, height))
            . M.mapKeysWith
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

horizontalAxisWidget :: GraphBuilder n
horizontalAxisWidget = asks $ fromMaybe Brick.emptyWidget . buildWidget
  where
    buildWidget st = do
        gd <- graphData st
        tz <- timezone st
        return Widget { hSize  = Brick.Greedy
                      , vSize  = Brick.Greedy
                      , render = renderImg gd tz
                      }

    renderImg graph tz = do
        image <- views heightAndWidthL $ \(w, h) ->
            let img = drawHorizontalAxisImage tz w (horizontalAxis graph)
            in  Vty.pad 0 0 0 (h - Vty.imageHeight img) img
        return (set imageL image emptyResult)

drawHorizontalAxisImage :: TimeZone -> Int -> [Time] -> Vty.Image
drawHorizontalAxisImage tz width values =
    let labels = generateLabelsTime tz values (0, width)
        axis   = buildImage labels $ \prev (pos, label) ->
            prev `horizJoin` buildNextBlock prev pos label
    in  axis `horizJoin` makeFill (width - Vty.imageWidth axis)
  where
    buildNextBlock = drawLabelledBlock . Vty.imageWidth

    buildImage labels f = foldl' f Vty.emptyImage labels

    makeFill w = Vty.charFill mempty '\9472' w 1

drawLabelledBlock :: Int -> Int -> Text -> Vty.Image
drawLabelledBlock offset current label =
    let width = fromIntegral $ max (current - offset) 0
        topBar =
                Vty.text mempty $ LT.replicate (width - 1) "\9472" `LT.snoc` '\9516'
        labelBar = Vty.text mempty $ prependSpace width $ LT.take
            (width - 1)
            (fromStrict label)
    in  topBar `vertJoin` labelBar

drawDefaultColumn :: Int -> Vty.Image
drawDefaultColumn (fromIntegral -> w) =
    Vty.text mempty $ prependSpace w "\9474"

prependSpace :: Int64 -> LText -> LText
prependSpace w = LT.justifyRight w ' '
