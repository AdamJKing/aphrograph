{-# LANGUAGE TypeFamilies #-}

module App
  ( runLoggingCurses
  , updateFrame
  )
where

import           Data.Traversable
import           System.IO
import           Data.Foldable
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Log
import           Control.Monad.Writer
import           Control.Monad.Loops
import           Control.Monad                  ( unless
                                                , void
                                                )
import           UI.NCurses
import           Graph
import           Graphite
import           Logging
import           Control.Monad.Trans.Control
import           Control.Monad.Catch
import           Control.Exception.Base         ( throwIO )
import           Time.Types                     ( toSeconds
                                                , Hours(..)
                                                )
import           Data.Text.Prettyprint.Doc
import           System.Environment

updateFrame :: [DataPoint] -> LoggingCurses ()
updateFrame metrics = do
  window          <- liftCurses $ setEcho False >> defaultWindow
  (width, height) <- liftCurses $ updateWindow window windowSize
  logMessage . pretty $ "width: " ++ show width ++ ", height: " ++ show height
  let (updates, logs) = unzip $ drawGraph (width, height) metrics
  logMessage . pretty $ "Number of updates: " ++ show (length updates)
  mapM_ (logMessage . pretty) logs
  liftCurses . updateWindow window $ sequence updates
  liftCurses $ render >> waitForClose window

waitForClose :: Window -> Curses ()
waitForClose window = waitFor window $ EventCharacter 'q'

waitFor :: Window -> Event -> Curses ()
waitFor window event = void . iterateUntil (event ==) $ waitForEvent
 where
  timeout      = Nothing
  waitForEvent = untilJust $ getEvent window timeout

