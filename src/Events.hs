{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

module Events where

import           App
import qualified App.Args                  as App

import qualified Brick.Types               as Brick

import           Control.Lens
import           Control.Monad.Log

import qualified Display.Graph             as Graph

import qualified Graphics.Vty.Input.Events as Vty

import           Graphite.Types

data AppEvent = UpdateEvent
    deriving ( Show, Eq )

type SystemEvent n = Brick.BrickEvent n AppEvent

data EventOutcome s = Update s | Continue s | Stop
    deriving ( Show, Eq )

pattern ExitKey :: Vty.Event
pattern ExitKey = Vty.EvKey (Vty.KChar 'q') []

class EventHandler e s (m :: * -> *) where
    handleEvent :: e -> m (EventOutcome s)

instance EventHandler (SystemEvent n) s (AppM s m) where
    handleEvent (Brick.VtyEvent ExitKey) = 
        logMessage "Recieved stop; quitting." >> return Stop

    handleEvent (Brick.AppEvent UpdateEvent) = do
        newGraphData <- updateGraphData
        updatedState <- asks (graphData .~ newGraphData)
        logMessage "Producing new state."
        return (Continue $ Active updatedState)

appEventHandler _ = reader (Continue . Active)

renderRequestFromArgs :: MonadReader ActiveState m => m GraphiteRequest
renderRequestFromArgs = do
    _from <- view (App.appArgs  . App.fromTime )
    _to <- view (App.appArgs  .App.toTime)
    _target <- view (App.appArgs  .App.targetArg)
    return (RenderRequest{..})

updateGraphData :: AppLike m => m (Graph.Graph Time Value)
updateGraphData = do
    request <- renderRequestFromArgs
    data' <- getMetrics request
    logMessage "Populating graph."
    return (Graph.extractGraph data')
