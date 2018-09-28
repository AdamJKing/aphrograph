{-# LANGUAGE OverloadedStrings #-}

module Graph
  ( normaliseRange
  , normalise
  , drawGraph
  )
where

import           Data.Decimal
import           Normalisation
import           Data.Text                      ( Text(..) )
import           Control.Monad
import           Control.Arrow
import           Control.Monad.State
import           Control.Monad.Log
import           Control.Monad.IO.Class
import           UI.NCurses
import           Graphite                       ( DataPoint(..) )
import           Data.Hourglass

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

toGraphableData :: DataPoint -> (Integer, Decimal)
toGraphableData = (fromIntegral . timeFrom . time) &&& value
  where timeFrom (Elapsed (Seconds t)) = t
