{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Events where

import           App
import qualified App.Args                  as App

import qualified Brick.Types               as Brick

import Fmt
import           Control.Lens
import           Control.Monad.Log

import qualified Display.Graph             as Graph

import qualified Graphics.Vty.Input.Events as Vty

import           Graphite.Types

type Logged m = MonadLog Text m

data AppEvent = UpdateEvent
    deriving ( Show, Eq )

type SystemEvent n = Brick.BrickEvent n AppEvent

data EventOutcome s = Continue s | Stop
    deriving ( Show, Eq )

pattern ExitKey :: Vty.Event
pattern ExitKey = Vty.EvKey (Vty.KChar 'q') []

appEventHandler :: (AppLike m, MonadIO m)
                => SystemEvent n
                -> AppState
                -> m (EventOutcome AppState)
appEventHandler _ (FailedAppState err) = 
    logMessage (fmt $ "Error occurred: " +|| err ||+ ".") >> return Stop
appEventHandler (Brick.VtyEvent ExitKey) _ =
    logMessage "Recieved stop; quitting." >> return Stop
appEventHandler (Brick.AppEvent UpdateEvent) (AppState ctxt) = do
    graphData <- updateGraphData
    logMessage "Producing new state."
    return $ Continue (AppState ( ctxt {graphData} ))
appEventHandler _ previousState = return (Continue previousState)

updateGraphData :: AppLike m => m (Graph.Graph Time Value)
updateGraphData = do
    fromTime <- view App.fromTime
    toTime <- view App.toTime
    target <- view App.targetArg
    data' <- getMetrics $ RenderRequest fromTime toTime target 
    logMessage "Populating graph."
    return (graphFromData data')
  where
    graphFromData = Graph.mkGraph . fmap Graph.extract