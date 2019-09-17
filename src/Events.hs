{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}

module Events where

import           App
import           App.Config
import qualified Brick.Types                   as Brick
import qualified Brick.Main                    as Brick
import           Control.Lens
import           Control.Monad.Log
import           Control.Monad.Except

import qualified Display.Graph                 as Graph
import           Display.Graph                  ( Graph )

import qualified Graphics.Vty.Input.Events     as Vty

import           Graphite.Types

data AppEvent = UpdateEvent
    deriving ( Show, Eq )

newtype SystemEvent n = SystemEvent ( Brick.BrickEvent n AppEvent )

data EventOutcome s = Update s | Continue s | Stop s
    deriving ( Show, Eq )

pattern ExitKey :: Vty.Event
pattern ExitKey = Vty.EvKey (Vty.KChar 'q') []

appEventHandler :: SystemEvent n -> ActiveState -> EventOutcome AppState
appEventHandler (SystemEvent (Brick.VtyEvent ExitKey)) = Stop . Active
appEventHandler (SystemEvent (Brick.AppEvent UpdateEvent)) = Update . Active
appEventHandler _ = Continue . Active

updateGraphData :: (IsString msg, MonadLog msg m, MonadGraphite m) => AppConfig -> m (Graph Time Value)
updateGraphData (AppConfig GraphiteConfig {..}) = do
    let request = RenderRequest { _from = fromTime, _to = toTime, _target = targetArg }
    data' <- getMetrics request
    logMessage "Populating graph."
    return (Graph.extractGraph data')

handleAppEvent
    :: ( MonadTrans t
       , m ~ t (Brick.EventM n)
       , MonadReader AppConfig m
       , MonadError AppError m
       , IsString msg
       , MonadLog msg m
       , MonadGraphite m
       )
    => EventOutcome ActiveState
    -> t (Brick.EventM n) (Brick.Next AppState)
handleAppEvent (Continue sameState) = lift (Brick.continue (Active sameState))
handleAppEvent (Stop     endState ) = lift (Brick.halt (Active endState))
handleAppEvent (Update previousState) =
    (ask >>= updateGraphData >>= applyUpdate) `catchError` (lift . Brick.halt . Failed)
    where applyUpdate ug = lift $ Brick.continue $ Active (previousState & graphData .~ ug)
