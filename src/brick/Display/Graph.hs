{-# LANGUAGE RecordWildCards #-}

module Display.Graph where

import qualified Display.Types as Display
import           Display.Axis
import           Control.Lens
import           Control.Lens.Each
import           Control.Monad.ST
import           Data.Monoid
import           Data.Array.ST
import           Normalisation
import           Data.Text                      ( Text(..) )
import           Control.Arrow
import           Control.Monad.State
import           Control.Monad.Log
import           Control.Monad.IO.Class
import           Graphite                       ( DataPoint(..) )
import           Data.Hourglass
import           Data.Array.Unboxed
import           Control.Monad

-- todo: account for no data scenario
newtype DisplayData = DisplayData (UArray (Integer, Integer) Bool)

toDisplayData :: [DataPoint] -> Display.Dimensions (Sum Integer) -> DisplayData
toDisplayData data' d@Display.Dimensions {..} = DisplayData $ runSTUArray $ do
  let transformedData =
        toHorizontalAxis data' (getSum width)
          `zip` toVerticalAxis data' (getSum height)
  emptyCanvas <- emptyGraphDisplay d
  renderToArray emptyCanvas transformedData
  return emptyCanvas
  where timeFrom (Elapsed (Seconds t)) = t

renderToArray :: (Ix i) => STUArray s (i, i) Bool -> [(i, i)] -> ST s ()
renderToArray target = mapM_ (\(x, y) -> writeArray target (x, y) True)

emptyGraphDisplay
  :: (Num i, Ix i) => Display.Dimensions (Sum i) -> ST s (STUArray s (i, i) Bool)
emptyGraphDisplay Display.Dimensions {..} =
  let origin = (mempty, mempty) & each %~ getSum
      end    = (width, height) & each %~ getSum
  in  newArray_ (origin, end)
