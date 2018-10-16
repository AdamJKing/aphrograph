{-# LANGUAGE LambdaCase #-}

module Events where

import qualified Brick.Main                    as Brick
import qualified Brick.Types                   as Brick
import           Control.Monad.Log
import qualified Graphite                       ( DataPoint )
import qualified Data.Text.Prettyprint.Doc     as PP
import qualified Control.Monad.IO.Class        as IO

newtype LoggingEvent = LoggingEvent {
  msg :: PP.Doc String
}

loggedEventHandler
  :: Handler IO (PP.Doc String)
  -> (  [Graphite.DataPoint]
     -> Brick.BrickEvent n LoggingEvent
     -> Brick.EventM n (Brick.Next [Graphite.DataPoint])
     )
loggedEventHandler handler state = \case
  Brick.AppEvent (LoggingEvent msg') -> do
    IO.liftIO $ handler msg'
    Brick.continue state
  e -> Brick.resizeOrQuit state e
