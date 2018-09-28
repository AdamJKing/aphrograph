{-# LANGUAGE TypeFamilies #-}

module App
  ( runLoggingCurses
  )
where

import           Display
import           Data.Traversable
import           System.IO
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

waitForClose :: Window -> Curses ()
waitForClose window = waitFor window $ EventCharacter 'q'

waitFor :: Window -> Event -> Curses ()
waitFor window event = void . iterateUntil (event ==) $ waitForEvent
 where
  timeout      = Nothing
  waitForEvent = untilJust $ getEvent window timeout
