{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}

module Display.Types where

import           Fmt
import           Text.Show
import           Data.Array.ST

data AppComponent = GraphView deriving (Eq, Ord, Show)

data Dimensions i = Dimensions { width :: !i, height :: !i } deriving (Eq, Show, Functor)

type Canvas = STUArray () (Int, Int) Char

dim :: (i, i) -> Dimensions i
dim (w, h) = Dimensions { width = w, height = h }

data DisplayError where
    DisplayTooSmall ::(Show i) => Dimensions i -> DisplayError
    ErrorDuringRender ::(Exception e) => e -> DisplayError

instance Show DisplayError where
    show (DisplayTooSmall dim') =
        fmt ("Could not use display as it was too small: dim=(" +|| dim' ||+ ")")
    show (ErrorDuringRender e) = fmt ("Error during rendering: err=" +|| e ||+ ".")

instance Exception DisplayError where
