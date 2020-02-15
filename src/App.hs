{-# LANGUAGE DataKinds #-}
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
import qualified Brick.Main as Brick
import qualified Brick.Types as Brick
import qualified Brick.Widgets.List as Brick
import Control.Lens.Combinators
import Control.Lens.Getter
import Control.Lens.Operators hiding ((??))
import Control.Lens.Prism (_Just)
import Control.Monad.Except
  ( MonadError,
    catchError,
    throwError,
  )
import Control.Monad.Log as Log
import qualified Display.Graph as Graph
import Display.GraphWidget
import Events
import Fmt
import qualified Graphics.Vty.Input.Events as Vty
import Graphite
import Graphite.Types
import Text.Show.Functions ()

type Logger m = Log.Handler m Text

newtype AppT m a = MkAppT {_unApp :: ReaderT App.Config (ExceptT App.Error (LoggingT Fmt.Builder m)) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadLog Fmt.Builder,
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
  fmap convertToRuntimeError $ runLoggingT ?? (logger . fmt) $ runExceptT $ usingReaderT conf $ _unApp action
  where
    convertToRuntimeError = either (error . ("Unhandled app error: " <>) . toText . displayException) id

constructDom :: App.CurrentState -> DisplayWidget App.Error
constructDom state' =
  view App.appData state' & \case
    (Left (App.FailedState err)) -> DisplayWidget $ Left (ErrorWidget err)
    (Right activeState) ->
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

instance MonadEventHandler AppEvent (AppT (Brick.EventM n)) where
  type EventS (AppT (Brick.EventM n)) = App.CurrentState

  handleEvent UpdateEvent s = do
    newState <- App.updateGraph updateGraph s `catchError` buildErrorState
    continue newState
    where
      buildErrorState err = return (App.CurrentState (Left (App.FailedState err)) (DisplayWidget (Left (ErrorWidget err))))

instance MonadEventHandler Vty.Event (AppT (Brick.EventM AppComponent)) where
  type EventS (AppT (Brick.EventM AppComponent)) = App.CurrentState

  handleEvent ExitKey = stop
  handleEvent (KeyDown 'm') = continue <=< App.toggleMetricsView
  handleEvent otherKeyPress = continue <=< App.setMetricsView (lift . Brick.handleListEventVi Brick.handleListEvent otherKeyPress)
