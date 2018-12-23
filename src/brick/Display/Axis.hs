module Display.Axis where

import           Labels
import qualified Graphics.Vty                  as Vty
import           Display.Graph
import           Control.Lens

toHorizontalAxis :: (Show x, Integral x) => Int -> Graph x y -> Vty.Image
toHorizontalAxis width NoData =
    Vty.string mempty $ "No Data" ++ replicate (width - 7) ' '
toHorizontalAxis width graph = Vty.string mempty . organiseLabels width $ over
    each
    Discrete
    (boundsX graph)
