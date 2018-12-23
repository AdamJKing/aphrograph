{-# LANGUAGE DeriveFunctor #-}

module Display.Types where

data AppComponent = GraphView deriving (Eq, Ord, Show)

data Dimensions i = Dimensions { width :: !i, height :: !i } deriving (Eq, Show, Functor)

dim :: (i, i) -> Dimensions i
dim (w, h) = Dimensions { width = w, height = h }
