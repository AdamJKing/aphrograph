{-# LANGUAGE FlexibleContexts #-}
module Display.Widgets where

import           Data.Foldable
import qualified Brick.Types                   as Brick
import           Display.Types
import           Control.Lens
import           Graphite
import           Display.Graph
import           App
import qualified Graphics.Vty                  as Vty
import           Graphics.Vty                   ( (<->)
                                                , (<|>)
                                                )
import           Data.Array.MArray
import           Data.Array.IArray
import           Data.Array.ST
import           Control.Monad.ST
import           Prelude                 hiding ( (<|>) )

graphWidget :: Graph Time Value -> Brick.Widget AppComponent
graphWidget _ = Brick.Widget
    { hSize  = Brick.Greedy
    , vSize  = Brick.Greedy
    , render = do
                   ctxt <- Brick.getContext
                   let width      = view Brick.availWidthL ctxt
                   let height     = view Brick.availHeightL ctxt

                   let graphImage = graphImageFor (Dimensions width height)
                   return $ set Brick.imageL graphImage Brick.emptyResult
    }
  where

    buildRow :: (IArray a Char) => a (Int, Int) Char -> Int -> Int -> Vty.Image
    buildRow picture y width =
        let pixel x = Vty.char mempty (picture ! (x, y))
        in  foldl (\row x -> row <|> pixel x) Vty.emptyImage [0 .. width]

    -- todo: Remove Dimensions arg in favour of array solution?
    buildImage :: (IArray a Char) => a (Int, Int) Char -> Dimensions Int -> Vty.Image
    buildImage picture Dimensions {..} =
        foldl (\total y -> total <-> buildRow picture y width) Vty.emptyImage [0 .. height]

    drawCanvas :: Dimensions Int -> ST s (STUArray s (Int, Int) Char)
    drawCanvas Dimensions {..} = newArray ((0, 0), (width, height)) ' '


    graphImageFor :: Dimensions Int -> Vty.Image
    graphImageFor dims = buildImage (runSTUArray (drawCanvas dims)) dims
