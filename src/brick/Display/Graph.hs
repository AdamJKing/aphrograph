{-# LANGUAGE RecordWildCards #-}

module Display.Graph where

import qualified Display.Types                 as Display
import           Display.Axis
import           Control.Lens
import           Control.Monad.ST
import           Data.Monoid
import           Data.Array.ST
import           Graphite                       ( DataPoint(..) )
import           Data.Array.Unboxed

-- todo: account for no data scenario
newtype DisplayData = DisplayData (UArray (Integer, Integer) Bool)
newtype GraphData = GraphData ([(Integer, Integer)])

makeGraphable :: [DataPoint] -> Display.Dimensions Integer -> GraphData
makeGraphable datapoints Display.Dimensions {..} =
  GraphData $ horizontalData `zip` verticalData
 where
  horizontalData = toHorizontalAxis datapoints (getSum width)
  verticalData   = toVerticalAxis datapoints (getSum height)

toDisplayData :: GraphData -> Display.Dimensions Integer -> DisplayData
toDisplayData (GraphData data') d = DisplayData $ runSTUArray $ do
  emptyCanvas <- canvas d
  renderToArray emptyCanvas data'
  return emptyCanvas

renderToArray :: (Ix i) => STUArray s i Bool -> [i] -> ST s ()
renderToArray target = mapM_ (\p -> writeArray target p True)

canvas :: (Num i, Ix i) => Display.Dimensions i -> ST s (STUArray s (i, i) Bool)
canvas Display.Dimensions {..} =
  let origin = (mempty, mempty) & each %~ getSum
      end    = (width, height) & each %~ getSum
  in  newArray_ (origin, end)
