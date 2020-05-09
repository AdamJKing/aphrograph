{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
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
import Control.Concurrent.Lifted
import Control.Lens.Combinators
import Control.Lens.Getter
import Control.Lens.Operators hiding ((??))
import Control.Monad.Base
import Control.Monad.Log as Log
import Control.Monad.Trans.Control
import qualified Display.Graph as Graph
import Display.GraphWidget
import Events
import Events.Types
import qualified Graphics.Vty.Input.Events as Vty
import Graphite
import Graphite.Types
import Text.Show.Functions ()

type Logger m = Log.Handler m LText

data AppSystem = AppSystem {_config :: App.Config, _eventCh :: Brick.BChan AppEvent}

makeLenses ''AppSystem

newtype AppT m a = MkAppT {_unApp :: ReaderT AppSystem (ExceptT App.Error (LoggingT LText m)) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadLog LText,
      MonadReader AppSystem,
      MonadError App.Error
    )

instance MonadTrans AppT where
  lift = MkAppT . lift . lift . lift

runApp :: Monad m => Brick.BChan AppEvent -> Logger m -> App.Config -> AppT m a -> m a
runApp chan logger conf action =
  fmap convertToRuntimeError $ runLoggingT ?? logger $ runExceptT $ usingReaderT (AppSystem conf chan) $ _unApp action
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
    conf <- view (App.config . App.graphiteConfig)
    response <- runGraphite conf (getMetrics req)
    case response of
      Right metrics -> return metrics
      Left err -> throwError (App.AppGraphiteError err)

  listMetrics = do
    conf <- view (App.config . App.graphiteConfig)
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
    App.GraphiteConfig {..} <- view (App.config . App.graphiteConfig)
    datapoints <- getMetrics $ RenderRequest _fromTime _toTime _targetArg
    return (Graph.mkGraph $ Graph.extract <$> datapoints)

triggerUpdate :: MonadIO m => Brick.BChan AppEvent -> AppT m ()
triggerUpdate ch = void $ fork $ do
  newGraph <- updateGraph
  liftIO (Brick.writeBChan ch (GraphUpdate newGraph))

instance MonadEventHandler AppEvent (AppT (Brick.EventM AppComponent)) where
  type EventS (AppT (Brick.EventM AppComponent)) = App.CurrentState

  handleEvent (GraphUpdate newGraph) st = do
    lift (Brick.invalidateCacheEntry GraphView)
    let newState = st & (App.active . App.graphData) .~ App.Present newGraph
    continue newState
  handleEvent TriggerUpdate (App.Active previousState) = do
    ch <- view App.eventCh
    triggerUpdate ch
    continue (App.Active (previousState & App.graphData .~ App.Pending))
  handleEvent _ previousState = continue previousState

instance MonadEventHandler Vty.Event (AppT (Brick.EventM AppComponent)) where
  type EventS (AppT (Brick.EventM AppComponent)) = App.CurrentState

  handleEvent ExitKey = stop
  handleEvent (KeyDown 'm') = continue <=< App.toggleMetricsView
  handleEvent otherKeyPress = continue <=< App.setMetricsView (lift . Brick.handleListEventVi Brick.handleListEvent otherKeyPress)

deriving instance MonadBase IO (AppT IO)

deriving instance MonadBaseControl IO (AppT IO)
