module Display.Types where

data Dimensions i = Dimensions { width :: i, height :: i } deriving (Eq, Show)

dim :: (i, i) -> Dimensions i
dim (w, h) = Dimensions { width = w, height = h }
