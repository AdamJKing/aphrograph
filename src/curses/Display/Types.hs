
module Display.Types where

import           Numeric.Natural
import qualified UI.NCurses                    as NC
import           Streaming

type RenderStream m data' = Stream Identity m (NC.Update data')

data Component m d = Component {
  children :: [Component m d],
  render :: RenderStream m d
}

data Dimensions = Dimensions {
  width :: Natural,
  height:: Natural
} deriving (Show, Eq)

data Point = Point { x :: Int, y :: Int }
