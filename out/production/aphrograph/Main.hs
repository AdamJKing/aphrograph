{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           Graphite
import           UI.NCurses

main :: IO ()
main =
  runCurses $ do
    setEcho False
    w <- defaultWindow
    m <- liftIO getCurrentValue
    updateWindow w $ do
      moveCursor 1 10
      drawString $ m
      moveCursor 3 10
      drawString "(press q to quit)"
      moveCursor 0 0
    render
    waitFor w (\ev -> ev == EventCharacter 'q' || ev == EventCharacter 'Q')

waitFor :: Window -> (Event -> Bool) -> Curses ()
waitFor w p = loop
  where
    loop = do
      ev <- getEvent w Nothing
      case ev of
        Nothing -> loop
        Just ev' ->
          if p ev'
            then return ()
            else loop
