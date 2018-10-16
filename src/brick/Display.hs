module Display where

import           Data.Monoid                    ( Sum(..) )
import           Data.Array.IArray
import           Control.Lens.Getter
import           Control.Lens.Setter
import           Data.Bool
import qualified Graphics.Vty                  as Vty
import qualified Display.Graph                 as Graph
import           Brick.Types                   as Brick
import           Graphite
import           Display.Types

renderGraphWidget :: [DataPoint] -> Brick.Widget ()
renderGraphWidget data' = Brick.Widget Greedy Greedy $ do
  ctx <- getContext
  let (w, h) = (view availWidthL ctx, view availHeightL ctx)
  let dim = Dimensions {width = Sum $ toInteger w, height = Sum $ toInteger h}
  let draw =
        toImage $ Graph.toDisplayData (Graph.makeGraphable data' dim) dim
  return $ set imageL draw emptyResult

toImage :: Graph.DisplayData -> Vty.Image
toImage (Graph.DisplayData graph) =
  let ((xMin, xMax), (yMin, yMax)) = bounds graph
  in  Vty.vertCat
      $   rowToImage
      <$> [ row
          | x <- [xMin .. xMax]
          , let row = [ graph ! (x, y) | y <- [yMin .. yMax] ]
          ]

rowToImage :: [Bool] -> Vty.Image
rowToImage row = Vty.horizCat $ Vty.char mempty . bool ' ' 'X' <$> row
