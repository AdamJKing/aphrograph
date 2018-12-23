
module Events where

import qualified Brick.Main                    as Brick
import qualified Brick.Types                   as Brick

import           Control.Monad.Trans.Class
import           Control.Lens
import           Display.Types
import           Data.Text.Prettyprint.Doc

import           Data.Text                      ( pack )
import           Graphite                       ( getMetricsForPast )
import           Args
import           App
import           Display.Graph                 as Graph
import           Control.Monad.Log
import           Display

data AppEvent = GraphRefresh

type AppEventHandler
  =  AppState
  -> Brick.BrickEvent AppComponent AppEvent
  -> LoggingT (Doc String) (Brick.EventM AppComponent) (Brick.Next AppState)

mkEventHandler :: AppArgs -> AppEventHandler
mkEventHandler AppArgs {..} state (Brick.AppEvent GraphRefresh) = do
  logMessage . pretty $ "Handling Graph Update!"
  maybeGraphView <- lift $ Brick.lookupViewport GraphView
  case maybeGraphView of
    Nothing -> lift $ Brick.continue state
    Just vp -> do
      latestData <- getMetricsForPast (pack _target) _time
      let graph = mkGraph $ extract <$> latestData
      logMessage . pretty $ "Graph Size: " ++ show (size graph)
      let targetSize = dim $ view Brick.vpSize vp
      let uiData     = normaliseGraph graph targetSize
      -- let uiLabels   = generateLabels (width targetSize) uiData
      lift $ Brick.continue $ AppState { appData = graph, ui_appData = uiData }

mkEventHandler AppArgs {..} state e = lift $ Brick.resizeOrQuit state e
