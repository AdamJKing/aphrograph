{-# LANGUAGE RankNTypes #-}

module Main where

import           Debug.Trace
import           Control.Monad
import           System.IO
import           UI.NCurses
import           ErrorHandling
import           Control.Monad.Catch
import           Control.Monad.Trans.Class      ( lift )
import           Data.Text.Prettyprint.Doc

import           Control.Monad.Log
import           Data.Typeable
import           Data.Int
import           App
import           Logging
import           Control.Monad.Writer
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Graph
import           Graphite
import           System.Environment
import           Time.Types
import qualified UI.NCurses                    as NC
import           Args
import           Display

main :: IO ()
main = do
  args <- getArgs
  let [targetArg, timeArg] = args
  let Just parsedTime      = parseTime timeArg
  withFileLogging runCurses $ app targetArg parsedTime
 where
  withFileLogging runner action =
    withFile "aphrograph.log" WriteMode $ \logfile ->
      runner . withFDHandler defaultBatchingOptions logfile 0.4 80 $ runLoggingT
        action


app :: (TimeInterval t) => String -> t -> LoggingT (Doc String) NC.Curses ()
app target parsedTime = do
  logMessage $ pretty "Starting app."
  display <- createDisplay
  step target parsedTime display

step
  :: (TimeInterval t)
  => String
  -> t
  -> DisplayT (LoggingT (Doc String) NC.Curses) [DataPoint]
  -> LoggingT (Doc String) NC.Curses ()
step target time display = do
  logMessage $ pretty "Performing single step."
  snapshot <- getMetricsForPast (pack target) time
  updateDisplay snapshot display
