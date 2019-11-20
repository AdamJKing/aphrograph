{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

module Events where

import qualified Brick.Types                   as Brick
import qualified Display.Graph                 as Graph
import qualified Graphics.Vty.Input.Events     as Vty
import qualified App.Config                    as App
import qualified App.State                     as App
import           Control.Monad.Log
import           Display.Graph                  ( Graph )
import           Graphite.Types
import           Display.Widgets
import           Control.Monad.Except           ( MonadError() )
import           App.Logging
import           Control.Lens.Setter
import           Control.Lens.Getter

data AppEvent = UpdateEvent | ExitEvent
    deriving ( Show, Eq )

type SystemEvent = Brick.BrickEvent AppComponent AppEvent

pattern ExitKey :: Vty.Event
pattern ExitKey = Vty.EvKey (Vty.KChar 'q') []

updateGraphData
    :: (MonadError App.Error m, Logger msg m, MonadReader App.Config m, MonadGraphite m) => m (Graph Time Value)
updateGraphData = do
    App.GraphiteConfig{..} <- view App.graphiteConfig
    let request = RenderRequest { _from = _fromTime, _to = _toTime, _target = _targetArg }
    data' <- getMetrics request
    logMessage "Populating graph."
    return (Graph.extractGraph data')

data EventHandler s m f = EventHandler {
    continue :: s -> m (f s),
    ignore :: s -> m (f s),
    stop :: s -> m (f s)
}

handleEvent
    :: (MonadGraphite m, MonadError App.Error m, MonadReader App.Config m, Logger msg m)
    => EventHandler App.ActiveState m f
    -> Brick.BrickEvent n AppEvent
    -> App.ActiveState
    -> m (f App.ActiveState)
handleEvent EventHandler {..} event state' = case event of
    (Brick.VtyEvent ExitKey    ) -> stop state'
    (Brick.AppEvent UpdateEvent) -> do
        newGraph <- updateGraphData
        continue (state' & App.graphData .~ newGraph)

    _ -> ignore state'
