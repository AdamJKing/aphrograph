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
import           Control.Lens
import           Control.Monad.Log
import           App
import Graphite


type Logged m = MonadLog Text m

data AppEvent = UpdateEvent deriving (Show, Eq)

data EventOutcome s = Continue s | Stop deriving (Show, Eq)

pattern ExitKey :: Vty.Event
pattern ExitKey = Vty.EvKey (Vty.KChar 'q') []

appEventHandler
  :: Brick.BrickEvent n AppEvent
  -> AppState
  -> AppT ( Brick.EventM AppComponent) (EventOutcome AppState)
appEventHandler (Brick.VtyEvent ExitKey) _ =
  logMessage "Recieved stop; quitting." >> return Stop
appEventHandler (Brick.AppEvent UpdateEvent) _ = Continue <$> updateGraphData
appEventHandler _ previousState = return (Continue previousState)

updateGraphData :: (MonadReader App.Args m, MonadGraphite m) => m AppState
updateGraphData = do
  fromTime <- view App.fromTime
  toTime <- view App.toTime
  target <- view App.targetArg
  data'  <- getMetricsForPast target fromTime toTime
  let newState = AppState (Graph.mkGraph (Graph.extract <$> data'))
  return newState