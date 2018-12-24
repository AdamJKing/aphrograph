module Display.Axis
  ( toHorizontalAxis
  )
where

import           Labels
import qualified Graphics.Vty                  as Vty
import           Display.Graph
import           Fmt

toHorizontalAxis :: (Show x, Integral x) => Int -> Graph x y -> Vty.Image
toHorizontalAxis width NoData = noDataLabel width
toHorizontalAxis width graph  = case genLabels of
  Left  _     -> noDataLabel width
  Right label -> Vty.string mempty . toString $ label
 where
  (x', x'') = boundsX graph
  genLabels =
    organiseLabels width $ generateLabels width (Discrete x', Discrete x'')

noDataLabel :: Int -> Vty.Image
noDataLabel width = Vty.string mempty . toString $ text
 where
  text :: Text
  text = fmt $ Fmt.padRightF (width - 7) ' ' ("No Data" :: Text)
