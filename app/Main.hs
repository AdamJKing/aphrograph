{-# LANGUAGE RankNTypes #-}

module Main where

import           Data.Typeable

import           Data.Int
import           App                            ( runLoggingCurses
                                                , updateFrame
                                                )

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Writer
import           Data.Text                      ( pack )
import           Graph
import           Graphite
import           System.Environment
import           Time.Types
import           UI.NCurses
import           Args

main :: IO ()
main = do
  args <- getArgs
  let [target, time] = args
  let Just parsedTime = parseTime time
  metrics <- getMetricsForPast (pack target) parsedTime
  runLoggingCurses $ updateFrame metrics
