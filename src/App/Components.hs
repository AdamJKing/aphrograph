{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module App.Components
  ( ErrorWidget (..),
    ComponentName,
    ComponentName' (..),
    ComponentM (_unCompT),
    MetricsBrowser (..),
    Browsable (..),
    GraphViewer (..),
    QuickOffset (..),
    TimeDialogue,
    ComponentState (..),
    chosenMetric,
    chosenTimeOffset,
    Dialogue (..),
    _Closed,
    _OpenOnMetrics,
    _OpenOnTime,
  updateDialogue)
where

import qualified Brick
import qualified Brick.Widgets.List as BWL
import qualified Brick.Widgets.List as Brick
import Control.Lens (makeLenses, makePrisms)
import Data.OpenUnion (Union, liftUnion)
import Data.Semigroup (Max (Max))
import Data.Vector (Vector)
import Display.Graph (Graph)
import Display.GraphWidget (GraphWidget, graphDisplayWidget)
import Display.TimeDialogueWidget (QuickOffset (..), TimeDialogue, TimeDialogueState, TimeFieldName)
import qualified Display.TimeDialogueWidget as TDW
import qualified Graphics.Vty as Vty
import qualified Graphite.Types as Graphite

data ComponentName' = GraphView | MetricsView
  deriving (Eq, Ord, Show)

type ComponentName = Union '[ComponentName', TimeFieldName]

newtype ErrorWidget e = ErrorWidget e deriving (Show)

data MetricsBrowser = MkMetricsBrowser
  { _browser :: BWL.List ComponentName Graphite.Metric,
    _width :: Int
  }

class Monad m => GraphViewer m where
  updateGraph :: Graphite.GraphiteRequest -> Graph Graphite.Time Graphite.Value -> m GraphWidget
  default updateGraph ::
    (MonadTrans t, m ~ t n, GraphViewer n) =>
    Graphite.GraphiteRequest ->
    Graph Graphite.Time Graphite.Value ->
    m GraphWidget
  updateGraph req graph = lift (updateGraph req graph)

class Browsable b m | b -> m where
  open :: Vector Graphite.Metric -> b
  scroll :: Vty.Event -> b -> m b
  selected :: b -> Maybe Graphite.Metric

newtype ComponentM a = CompM {_unCompT :: Brick.EventM ComponentName a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadFail)

instance GraphViewer ComponentM where
  updateGraph ctx newGraph = do
    CompM $ Brick.invalidateCacheEntry (liftUnion GraphView)
    return (graphDisplayWidget ctx newGraph)

instance Browsable MetricsBrowser ComponentM where
  open metrics =
    let (Max width) = foldMap (Max . Graphite.metricLength) metrics
     in MkMetricsBrowser (Brick.list (liftUnion MetricsView) metrics 1) width

  scroll event (MkMetricsBrowser browser width) = CompM $ do
    updated <- Brick.handleListEventVi Brick.handleListEvent event browser
    return (MkMetricsBrowser updated width)

  selected (MkMetricsBrowser browser _) = snd <$> Brick.listSelectedElement browser

data ComponentState = ComponentState
  { _chosenMetric :: Graphite.Metric,
    _chosenTimeOffset :: TimeDialogueState
  }

makeLenses ''ComponentState

data Dialogue ns e = OpenOnMetrics MetricsBrowser | OpenOnTime (TimeDialogue ns e) | Closed

makePrisms ''Dialogue

updateDialogue :: Brick.BrickEvent ComponentName e -> TimeDialogue ComponentName e -> ComponentM (TimeDialogue ComponentName e)
updateDialogue e d = CompM (TDW.updateDialogue e d)