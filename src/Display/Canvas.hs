{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Display.Canvas where

import           Data.Array.MArray
import           Data.Array.ST
import           Control.Monad.ST
import           Display.Types
import qualified Graphics.Vty                  as Vty


newtype Canvas s = Canvas { underlying :: STUArray s (Int, Int) Char }

newtype CanvasUpdate s a = CanvasUpdate ( ReaderT (Canvas s) (ST s) a )
  deriving (Functor, Applicative, Monad, MonadReader (Canvas s))

xBounds :: CanvasUpdate s (Int, Int)
xBounds = CanvasUpdate . ReaderT $ \canvas -> do
  (_, (w, _)) <- getBounds (underlying canvas)
  return (0, w)

yBounds :: CanvasUpdate s (Int, Int)
yBounds = CanvasUpdate . ReaderT $ \canvas -> do
  (_, (_, h)) <- getBounds (underlying canvas)
  return (0, h)

readPixel :: (Int, Int) -> CanvasUpdate s Char
readPixel point =
  CanvasUpdate . ReaderT $ \(Canvas underlying) -> readArray underlying point


newCanvas :: Dimensions Int -> ST s (Canvas s)
newCanvas Dimensions {..} = Canvas <$> newArray ((0, 0), (width, height)) ' '

paintPoint :: (Int, Int) -> CanvasUpdate s ()
paintPoint p =
  CanvasUpdate . ReaderT $ \(Canvas underlying) -> writeArray underlying p 'X'

renderToImage :: CanvasUpdate s Vty.Image
renderToImage = do
  xb   <- diff <$> xBounds
  yb   <- diff <$> yBounds

  rows <- forM [0 .. yb] $ \y -> do
    row <- forM [0 .. xb] (readPixel . (, y))
    return (Vty.horizCat $ Vty.char mempty <$> row)

  return (Vty.vertCat rows)
  where diff (a, b) = b - a
