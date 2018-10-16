module Display.Types where

import Data.Monoid

data Dimensions i = Dimensions { width :: Sum i, height :: Sum i }