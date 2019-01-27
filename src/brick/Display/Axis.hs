{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
module Display.Axis where

import qualified Graphics.Vty                  as Vty
import           Graphics.Vty                   ( (<->)
                                                -- , (<|>)
                                                )
import           Display.Types                  ( DisplayError(..)
                                                , Dimensions(..)
                                                )
-- import qualified Data.Text.Lazy                as TL
import qualified Data.Text                     as T
import qualified Data.Foldable                 as FL
import           Labels

newtype VerticalAxis = Vertical Axis
newtype HorizontalAxis = Horizontal Axis

data Axis = Axis Vty.Image (Maybe Int)

-- doesn't limit by width!
mkVerticalAxis :: (Ord a, Foldable t) => Dimensions Int -> t a -> Axis
mkVerticalAxis d@(Dimensions { height }) data'
  | height `mod` 5 /= 0 = mkVerticalAxis (d { height = height - 1 }) data'
  | otherwise           = generateLabel labelWidth
  where labelWidth = height `div` 5

mkHorizontalAxis
  :: (Show i, Integral i, Foldable t) => Dimensions i -> t a -> Either DisplayError HorizontalAxis
mkHorizontalAxis dim labels | FL.null labels = Right (HorizontalAxis (Vty.text mempty "No Data"))
                            | fromIntegral (width dim) < largestLabel = Left (DisplayTooSmall dim)
                            | otherwise = Right (buildLabel labelSize labels)
 where
  labelSize    = fromIntegral (width dim) `div` length labels
  largestLabel = FL.maximum (T.length <$> labels)
  labels       = generateLabels (fromIntegral (height dim)) (FL.minimum data', FL.maximum data')

paddedImg :: Int -> Text -> Vty.Image
paddedImg w = Vty.text' mempty . T.justifyRight w ' '
