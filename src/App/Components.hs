{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module App.Components
  ( ErrorWidget (..),
    ComponentName (..),
    ComponentM (_unCompT),
    MetricsBrowser (..),
    Browsable (..),
    GraphViewer (..),
    QuickOffset (..),
    TimeDialogue (..),
    timeDialogue,
    ComponentState (..),
    chosenMetric,
    chosenTimeOffset,
    updateDialogue,
  )
where

import qualified Brick
import Brick.Widgets.Dialog (Dialog)
import qualified Brick.Widgets.Dialog as Brick
import qualified Brick.Widgets.List as BWL
import qualified Brick.Widgets.List as Brick
import Control.Lens (makeLenses)
import Data.Semigroup (Max (Max))
import Data.Vector (Vector)
import Display.Graph (Graph)
import Display.GraphWidget (GraphWidget, graphDisplayWidget)
import qualified Graphics.Vty as Vty
import qualified Graphite.Types as Graphite

newtype ErrorWidget e = ErrorWidget e deriving (Show)

data MetricsBrowser = MkMetricsBrowser
  { _browser :: (BWL.List ComponentName Graphite.Metric),
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

data ComponentName = GraphView | MetricsView
  deriving (Eq, Ord, Show)

newtype ComponentM a = CompM {_unCompT :: Brick.EventM ComponentName a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadFail)

instance GraphViewer ComponentM where
  updateGraph ctx newGraph = do
    CompM $ Brick.invalidateCacheEntry GraphView
    return (graphDisplayWidget ctx newGraph)

instance Browsable MetricsBrowser ComponentM where
  open metrics =
    let (Max width) = foldMap (Max . Graphite.metricLength) metrics
     in MkMetricsBrowser (Brick.list MetricsView metrics 1) width

  scroll event (MkMetricsBrowser browser width) = CompM $ do
    updated <- Brick.handleListEventVi Brick.handleListEvent event browser
    return (MkMetricsBrowser updated width)

  selected (MkMetricsBrowser browser _) = snd <$> Brick.listSelectedElement browser

data QuickOffset = FifteenMins | OneHour | TwelveHours | TwentyFourHours | SevenDays
  deriving (Enum)

data TimeDialogue = OpenDialogue (Dialog QuickOffset)

timeDialogue :: QuickOffset -> TimeDialogue
timeDialogue default' =
  let title = "Time Picker"
      options = do
        option <- [toEnum 0 ..]
        return (nameOffset option, option)
   in OpenDialogue $ Brick.dialog (Just title) (Just (fromEnum default', options)) 25
  where
    nameOffset FifteenMins = "-15m"
    nameOffset OneHour = "-1h"
    nameOffset TwelveHours = "-12h"
    nameOffset TwentyFourHours = "-24h"
    nameOffset SevenDays = "-7d"

updateDialogue :: Vty.Event -> TimeDialogue -> ComponentM TimeDialogue
updateDialogue ev (OpenDialogue dialogue) = OpenDialogue <$> (CompM $ Brick.handleDialogEvent ev dialogue)

data ComponentState = ComponentState
  { _chosenMetric :: Graphite.Metric,
    _chosenTimeOffset :: QuickOffset
  }

makeLenses ''ComponentState
