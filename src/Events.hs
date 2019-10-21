{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

module Events where

import qualified Brick.Types                   as Brick
import qualified Brick.Main                    as Brick
import           Brick.Types                    ( BrickEvent )
import           Control.Monad.Log
import qualified Display.Graph                 as Graph
import           Display.Graph                  ( Graph )
import qualified Graphics.Vty.Input.Events     as Vty
import           Graphite.Types
import qualified App.Config                    as App
import           App

data AppEvent = UpdateEvent | ExitEvent
    deriving ( Show, Eq )

newtype SystemEvent n = SystemEvent ( Brick.BrickEvent n AppEvent )

data EventOutcome s = Continue s | Stop s
    deriving ( Show, Eq )

pattern ExitKey :: Vty.Event
pattern ExitKey = Vty.EvKey (Vty.KChar 'q') []

updateGraphData :: (IsString msg, MonadLog msg m, MonadGraphite m) => App.Config -> m (Graph Time Value)
updateGraphData (App.Config App.GraphiteConfig {..}) = do
    let request = RenderRequest { _from = fromTime, _to = toTime, _target = targetArg }
    data' <- getMetrics request
    logMessage "Populating graph."
    return (Graph.extractGraph data')

handleBrickEvents :: BrickEvent n AppEvent -> s -> AppM (Brick.Next s)
handleBrickEvents (Brick.VtyEvent ExitKey    ) = App.liftEventM . Brick.halt
handleBrickEvents (Brick.AppEvent UpdateEvent) = App.liftEventM . Brick.continue
handleBrickEvents _                            = App.liftEventM . Brick.continue
