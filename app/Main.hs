
module Main where

import           Graphics.Vty (defaultConfig, mkVty)
import           Aphrograph (aphrograph)

main :: IO ()
main = aphrograph =<< mkVty defaultConfig