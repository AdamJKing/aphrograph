{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

module Events where

import qualified Brick.Types                   as Brick
import qualified Display.Graph                 as Graph
import qualified Graphics.Vty.Input.Events     as Vty
import qualified App.Args                      as App
import           Graphite                hiding ( time )
import           Control.Lens
import           Control.Monad.Log
import           App


type Logged m = MonadLog Text m

data AppEvent = UpdateEvent deriving (Show, Eq)

data EventOutcome s = Continue s | Stop deriving (Show, Eq)

pattern ExitKey :: Vty.Event
pattern ExitKey = Vty.EvKey (Vty.KChar 'q') []

appEventHandler
  :: (MonadGraphite m, Logged m, MonadReader App.Args m)
  => Brick.BrickEvent n AppEvent
  -> AppState
  -> m (EventOutcome AppState)
appEventHandler (Brick.VtyEvent ExitKey) _ =
  logMessage "Recieved stop; quitting." >> return Stop
appEventHandler (Brick.AppEvent UpdateEvent) _ = do
  time   <- view App.timeArg
  target <- view App.targetArg
  data'  <- getMetricsForPast target time
  let newState = AppState (Graph.mkGraph (Graph.extract <$> data'))
  return (Continue newState)
appEventHandler _ previousState = return (Continue previousState)
