{-# LANGUAGE LambdaCase #-}

module Events where

import qualified Brick.Main                    as Brick
import qualified Brick.Types                   as Brick
import           System.IO                      ( withFile
                                                , IOMode(WriteMode)
                                                )
import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad.Log
import           Data.Text                      ( Text
                                                , pack
                                                )
import qualified Graphite                       ( DataPoint )
import           System.Environment             ( getArgs )
import           Time.Types
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
