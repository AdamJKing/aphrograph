
module Events where

import qualified Brick.Main                    as Brick
import qualified Brick.Types                   as Brick
import           Control.Lens
import           Display.Types
import           Graphite                       ( getMetricsForPast
                                                , timeAsSeconds
                                                )
import           Args
import           App
import           Display.Graph                 as Graph
import           Control.Monad.Log
import           Fmt

data AppEvent = GraphRefresh

type AppEventHandler
  =  AppState
  -> Brick.BrickEvent AppComponent AppEvent
  -> LoggingT Text (Brick.EventM AppComponent) (Brick.Next AppState)

-- mkEventHandler :: AppArgs -> AppEventHandler
-- mkEventHandler AppArgs {..} appState (Brick.AppEvent GraphRefresh) = do
--   logMessage "Handling Graph Update!"
--   maybeGraphView <- lift $ Brick.lookupViewport GraphView
--   case maybeGraphView of
--     Nothing -> lift $ Brick.continue appState
--     Just vp -> do
--       latestData <- getMetricsForPast _target _time
--       let graph = mkGraph $ extract <$> latestData

--       logMessage $ "Graph Size: " +|| size graph |+ ""

--       let targetSize = dim $ view Brick.vpSize vp

--       let uiData     = normaliseGraph graph targetSize

--       xl <- xLabels targetSize graph
--       yl <- yLabels targetSize graph

--       lift . Brick.continue $ AppState { _appData = graph, _ui = UI uiData xl yl }

--  where
--   xLabels targetSize graph = do
--     let rng    = over each (Discrete . timeAsSeconds) (Graph.boundsX graph)
--     let labels = generateLabels (width targetSize) rng
--     logMessage $ "UI Labels (X): " +|| labels ||+ ""
--     return labels

--   yLabels targetSize graph = do
--     let rng    = Graph.boundsY graph
--     let labels = generateLabels (width targetSize) rng
--     logMessage $ "UI Labels (X): " +|| labels ||+ ""
--     return labels

-- mkEventHandler AppArgs {..} appState e = lift $ Brick.resizeOrQuit appState e
