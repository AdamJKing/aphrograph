{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

module Events where

import qualified Brick.Types                   as Brick
import Control.Monad.Except
import qualified Display.Graph                 as Graph
import qualified Graphics.Vty.Input.Events     as Vty
import qualified App.Args                      as App
import           Control.Lens
import           Control.Monad.Log
import           App
import Graphite
import Graphite.Types
import           Data.Time.LocalTime


type Logged m = MonadLog Text m

data AppEvent = UpdateEvent deriving (Show, Eq)

type SystemEvent n = Brick.BrickEvent n AppEvent

data EventOutcome s = Continue s | Stop deriving (Show, Eq)

pattern ExitKey :: Vty.Event
pattern ExitKey = Vty.EvKey (Vty.KChar 'q') []

appEventHandler
  :: (MonadError AppError m, MonadGraphite m, MonadReader App.Args m, MonadLog Text m, MonadIO m) => SystemEvent n
  -> AppState
  -> m (EventOutcome AppState)
appEventHandler (Brick.VtyEvent ExitKey) _ =
  logMessage "Recieved stop; quitting." >> return Stop
appEventHandler (Brick.AppEvent UpdateEvent) _ = Continue <$> updateGraphData
appEventHandler _ previousState = return (Continue previousState)

updateGraphData :: (MonadError AppError m, MonadReader App.Args m, MonadGraphite m, MonadIO m) => m AppState
updateGraphData = do
  fromTime <- view App.fromTime
  toTime <- view App.toTime
  target <- view App.targetArg
  (GraphiteResponse data')  <- getMetrics $ RenderRequest fromTime toTime target

  AppState (graphFromData data') <$> liftIO getCurrentTimeZone
  where graphFromData = Graph.mkGraph . fmap Graph.extract