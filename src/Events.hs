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
import           Graphite
import           Graphite.Types
import           Display.Widgets
import           Control.Monad.Except           ( MonadError(throwError) )
import           App.Logging
import           Control.Lens.Setter

data AppEvent = UpdateEvent | ExitEvent
    deriving ( Show, Eq )

type SystemEvent = Brick.BrickEvent AppComponent AppEvent

pattern ExitKey :: Vty.Event
pattern ExitKey = Vty.EvKey (Vty.KChar 'q') []

updateGraphData
    :: (MonadError App.Error m, MonadIO m, Logger msg m, MonadReader App.GraphiteConfig m) => m (Graph Time Value)
updateGraphData = do
    App.GraphiteConfig {..} <- ask
    data' <- runGraphite _graphiteUrl
                         (getMetrics $ RenderRequest { _from = _fromTime, _to = _toTime, _target = _targetArg })
    logMessage "Populating graph."
    either (throwError . App.AppGraphiteError) (return . Graph.extractGraph) data'

data EventHandler s m f = EventHandler {
    continue :: s -> m (f s),
    ignore :: s -> m (f s),
    stop :: s -> m (f s)
}

handleEvent
    :: (MonadError App.Error m, MonadIO m, MonadReader App.GraphiteConfig m, Logger msg m)
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
