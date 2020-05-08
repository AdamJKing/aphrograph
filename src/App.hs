{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module App where

import App.Components
import qualified App.Config as App
import qualified App.State as App
import qualified Brick.BChan as Brick
import qualified Brick.Main as Brick
import qualified Brick.Types as Brick
import qualified Brick.Widgets.List as Brick
import Control.Lens.Combinators
import Control.Lens.Getter
import Control.Lens.Operators hiding ((??))
import Control.Monad.Log as Log
import qualified Display.Graph as Graph
import Display.GraphWidget
import Events
import Events.Types
import qualified Graphics.Vty.Input.Events as Vty
import Graphite
import Graphite.Types
import Text.Show.Functions ()

type Logger m = Log.Handler m LText

newtype AppT m a = MkAppT {_unApp :: ReaderT App.Config (ExceptT App.Error (LoggingT LText m)) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadLog LText,
      MonadReader App.Config,
      MonadError App.Error
    )

instance MonadTrans AppT where
  lift = MkAppT . lift . lift . lift

instance Monad m => App.Configured App.Config (AppT m) where
  getConfig = view
  {-# INLINE getConfig #-}

runApp :: Monad m => Logger m -> App.Config -> AppT m a -> m a
runApp logger conf action =
  fmap convertToRuntimeError $ runLoggingT ?? logger $ runExceptT $ usingReaderT conf $ _unApp action
  where
    convertToRuntimeError = either (error . ("Unhandled app error: " <>) . toText . displayException) id

constructDom :: App.CurrentState -> DisplayWidget App.Error
constructDom (App.Failed (App.FailedState err)) = DisplayWidget $ Left (ErrorWidget err)
constructDom (App.Active activeState) =
  DisplayWidget $ Right $
    DefaultDisplay
      { dataDisplay = graphDisplayWidget (activeState ^. App.graphData) (activeState ^. App.timezone),
        metricBrowser = activeState ^? toMetricBrowser
      }
  where
    toMetricBrowser = App.metricsView . _Just . to MetricsBrowser

instance MonadIO m => MonadGraphite (AppT m) where
  getMetrics req = do
    conf <- view App.graphiteConfig
    response <- runGraphite conf (getMetrics req)
    case response of
      Right metrics -> return metrics
      Left err -> throwError (App.AppGraphiteError err)

  listMetrics = do
    conf <- view App.graphiteConfig
    response <- runGraphite conf listMetrics
    case response of
      Right metrics -> return metrics
      Left err -> throwError (App.AppGraphiteError err)

instance MonadOutcome Brick.Next (AppT (Brick.EventM n)) where
  continue = lift . Brick.continue
  stop = lift . Brick.halt

type instance EventF (AppT (Brick.EventM n)) = Brick.Next

instance MonadIO m => GraphViewer (AppT m) where
  updateGraph = do
    fromTime <- App.getConfig (App.graphiteConfig . App.fromTime)
    toTime <- App.getConfig (App.graphiteConfig . App.toTime)
    target <- App.getConfig (App.graphiteConfig . App.targetArg)
    datapoints <- getMetrics $ RenderRequest fromTime toTime target
    return (Graph.mkGraph $ Graph.extract <$> datapoints)

writeEvent :: MonadIO m => Brick.BChan e -> e -> AppT m ()
writeEvent ch e = liftIO (Brick.writeBChan ch e)

triggerUpdate :: App.ActiveState -> AppT m ()
triggerUpdate st = do
  let ch = st ^. App.eventCh
  newGraph <- updateGraph
  ch `writeEvent` GraphUpdate newGraph

instance MonadEventHandler AppEvent (AppT (Brick.EventM AppComponent)) where
  type EventS (AppT (Brick.EventM AppComponent)) = App.CurrentState

  handleEvent (GraphUpdate newGraph) st = do
    lift (Brick.invalidateCacheEntry GraphView)
    let newState = st & (App.active . App.graphData) .~ (App.Present newGraph)
    continue newState
  handleEvent TriggerUpdate previousState =
    triggerUpdate previousState
      >> continue (previousState & (App.active . App.graphData) .~ App.Pending)

instance MonadEventHandler Vty.Event (AppT (Brick.EventM AppComponent)) where
  type EventS (AppT (Brick.EventM AppComponent)) = App.CurrentState

  handleEvent ExitKey = stop
  handleEvent (KeyDown 'm') = continue <=< App.toggleMetricsView
  handleEvent otherKeyPress = continue <=< App.setMetricsView (lift . Brick.handleListEventVi Brick.handleListEvent otherKeyPress)
