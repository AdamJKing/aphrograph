{-# LANGUAGE LambdaCase #-}

module Events where

import qualified Brick.Main                    as Brick
import qualified Brick.Types                   as Brick
import qualified Control.Monad.IO.Class        as IO
import           Control.Monad.Trans.Class
import           Control.Lens
import           Display.Types
import           Data.Text.Prettyprint.Doc
import           Data.Maybe
import           Data.Text                      ( pack )
import           Graphite                       ( getMetricsForPast )
import           Args
import           App
import           Display.Graph                 as Graph
import           Control.Monad.Log
import           Display

data GraphRefreshEvent = GraphRefresh

produceGraphRefresh
  :: AppArgs
  -> Handler IO (Doc String)
  -> Brick.EventM Components (Graph Integer Integer)
produceGraphRefresh args handler = runLoggingT
  (do
    vp     <- lift $ Brick.lookupViewport GraphView
    points <- getMetricsForPast (pack (_target args)) (_time args)

    let graph = mkGraph $ extract <$> points

    logMessage . pretty $ "Graph Size: " ++ show (size graph)
    logMessage . pretty $ show vp

    let targetSize =
          dim $ over each toInteger (view Brick.vpSize $ fromJust vp)

    logMessage . pretty $ show targetSize

    normalisedGraph <- normaliseGraph graph targetSize

    logMessage . pretty $ "Normalised Graph Size: " ++ show
      (size normalisedGraph)

    return normalisedGraph
  )
  eventLogHandler
  where eventLogHandler = IO.liftIO . handler

eventHandler
  :: Handler IO (Doc String)
  -> AppArgs
  -> (  Graph Integer Integer
     -> Brick.BrickEvent Components GraphRefreshEvent
     -> Brick.EventM Components (Brick.Next (Graph Integer Integer))
     )
eventHandler handler args state = \case
  Brick.AppEvent GraphRefresh ->
    produceGraphRefresh args handler >>= Brick.continue
  e -> Brick.resizeOrQuit state e
