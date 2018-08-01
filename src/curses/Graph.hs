{-# LANGUAGE OverloadedStrings #-}

module Graph
    ( normalise
    , drawGraph
    )
where

import           Data.Text                      ( Text(..) )
import           Control.Monad
import           Control.Arrow
import           Control.Monad.State
import           Control.Monad.Log
import           Control.Monad.IO.Class
import           Data.Foldable
import           UI.NCurses
import           Graphite                       ( DataPoint(..) )
import           Data.Hourglass                 ( Elapsed(..)
                                                , Seconds(..)
                                                )

genUpdate :: (Integer, Integer) -> (Update (), String)
genUpdate (x, y) = (update, message)
  where
    message = "drawing at " ++ show x ++ ", " ++ show y
    update  = moveCursor y x >> drawGlyph glyphDiamond

drawGraph :: (Integer, Integer) -> [DataPoint] -> [(Update (), String)]
drawGraph _               []  = mempty
drawGraph (width, height) dps = genUpdate <$> normalised
  where
    toFloat    = fromInteger :: Integer -> Float
    (xs, ys)   = unzip $ map toGraphableData dps
    normalised = zip
        (round <$> normaliseRange (map toFloat xs) (1, toFloat (height - 1)))
        (round <$> normaliseRange ys (1, fromInteger (width - 1)))


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

toGraphableData :: DataPoint -> (Integer, Float)
toGraphableData = (fromIntegral . timeFrom . time) &&& value
    where timeFrom (Elapsed (Seconds t)) = t

normaliseRange :: (Show n, Fractional n, Ord n) => [n] -> (n, n) -> [n]
normaliseRange ns (smallest, largest) =
    normalise (minimum ns, maximum ns) (smallest, largest) <$> ns
