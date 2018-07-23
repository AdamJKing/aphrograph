{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Graph
    ( normalise
    , normaliseList
    , drawGraph
    )
where

import           Data.Text                      ( Text(..) )
import           Control.Monad
import           Control.Monad.Log
import           Control.Monad.Writer
import           Data.Text.Prettyprint.Doc
import           Control.Monad.IO.Class
import           Data.Foldable
import           UI.NCurses
import           Graphite                       ( DataPoint(..) )
import           Data.Hourglass                 ( Elapsed(..)
                                                , Seconds(..)
                                                )

update :: (Integer, Integer) -> (Update (), String)
update (x, y) = (update, message)
  where
    update  = moveCursor x y >> drawGlyph glyphDiamond
    message = "drawing at " ++ show x ++ ", " ++ show y

drawGraph :: [DataPoint] -> (Update (), String)
drawGraph [] = update (0, 0)
drawGraph dps = (foldl1 (>>) updates, unlines logs)
    where (updates, logs) =  unzip . map update . toGraphableData $ dps

normaliseErr :: (Show a) => a -> (a, a) -> String
normaliseErr i a =
    "normalised number must be in range. ["
        ++ show i
        ++ " was not in "
        ++ show a

-- (i - min(a)) * (max(b) - min(b))
-- --------------------------------  + min(b)
--      (max(a) - min(a))
normalise :: (Ord a, Show a, Fractional a) => (a, a) -> (a, a) -> a -> a
normalise a b i
    | (max' a < i) || (min' a > i) = error $ normaliseErr i a
    | max' a == min' a = error $ "Arg was not a range " ++ show a
    | a == b = i
    | otherwise = (i - min' a)
    * ((max' b - min' b) / (max' a - min' a))
    + min' b
  where
    max' (x, y) = max x y
    min' (x, y) = min x y

normaliseList :: (Show a, Ord a, Fractional a) => (a, a) -> (a, a) -> [a] -> [a]
normaliseList r l = fmap $ normalise (lMin, lMax) r
  where
    lMin = minimum l
    lMax = maximum l

toGraphableData :: [DataPoint] -> [(Integer, Integer)]
toGraphableData [] = []
toGraphableData (DataPoint { value = v, time = Elapsed (Seconds t) } : ds) =
    (round v, fromIntegral t) : toGraphableData ds
