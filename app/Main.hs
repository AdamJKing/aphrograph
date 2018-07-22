module Main where

import           App                            ( runLoggingCurses
                                                , updateFrame
                                                )
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Writer
import           Graphite
import           UI.NCurses
import           Graph
import           Time.Types                     ( toSeconds
                                                , Minutes(..)
                                                )

main :: IO ()
main = runLoggingCurses updateFrame
