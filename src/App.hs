{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module App where

import App.Components
  ( AppWidget (DefaultDisplay, dataDisplay, metricBrowser),
    ComponentName (GraphView),
    DisplayWidget (..),
    ErrorWidget (ErrorWidget),
    GraphViewer (..),
  )
import qualified App.Config as App
import qualified App.State as App
import qualified Brick.BChan as Brick
import qualified Brick.Main as Brick
import qualified Brick.Types as Brick
import Control.Concurrent (Chan)
import Control.Concurrent.Lifted (fork)
import Control.Lens (makeLenses, view, views, (^.))
import Control.Monad.Base (MonadBase)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Logger (LogLine, LoggingT, MonadLogger, logInfo, mapLoggingT, runChanLoggingT)
import Control.Monad.Morph (MFunctor (..))
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Display.Graph as Graph
import Display.GraphWidget (graphDisplayWidget)
import Events.Types
  ( AppEvent (GraphUpdate),
    MonadEvent (..),
    MonadOutcome (..),
  )
import Formatting (int, sformat, (%))
import Graphite (runGraphite)
import Graphite.Types as Graphite
  ( GraphiteRequest (RenderRequest),
    MonadGraphite (..),
  )

newtype AppChan e = AppChan (Brick.BChan e)

instance MonadIO m => MonadEvent AppChan m where
  writeEvent (AppChan ch) = liftIO . Brick.writeBChan ch

data AppSystem' ch = AppSystem {_config :: App.Config, _eventCh :: ch AppEvent}

type AppSystem = AppSystem' AppChan

makeLenses ''AppSystem'

newtype AppT m a = MkAppT {_unApp :: ReaderT AppSystem (LoggingT (ExceptT App.Error m)) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader AppSystem,
      MonadError App.Error,
      MonadBase b,
      MonadBaseControl b,
      MonadLogger
    )

instance MFunctor AppT where
  hoist nat = MkAppT . hoist (mapLoggingT (hoist nat)) . _unApp

instance MonadTrans AppT where
  lift = MkAppT . lift . lift . lift

runApp :: (MonadIO m, MonadFail m) => Chan LogLine -> AppSystem -> AppT m a -> m a
runApp logChan deps = failOnError . runChanLoggingT logChan . usingReaderT deps . _unApp
  where
    failOnError res =
      runExceptT res >>= \case
        (Right a) -> return a
        (Left e) -> fail (displayException e)

constructDom :: App.CurrentState m -> DisplayWidget m App.Error
constructDom (App.Failed (App.FailedState err)) = DisplayWidget $ Left (ErrorWidget err)
constructDom (App.Active activeState) =
  DisplayWidget $
    Right $
      DefaultDisplay
        { dataDisplay = activeState ^. App.graphData,
          metricBrowser = activeState ^. App.metricsView
        }

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
      Right metrics ->
        $(logInfo) (sformat ("Returned " % int % " metrics from graphite") (length metrics))
          >> return metrics
      Left err -> throwError (App.AppGraphiteError err)

instance MonadOutcome (AppT (Brick.EventM n)) where
  type EventF (AppT (Brick.EventM n)) = Brick.Next

  continue = lift . Brick.continue
  {-# INLINEABLE continue #-}
  stop = lift . Brick.halt
  {-# INLINEABLE stop #-}

instance GraphViewer (AppT (Brick.EventM ComponentName)) where
  triggerUpdate metric =
    inThread $ do
      ch <- view App.eventCh
      newGraph <- fetchGraph
      writeEvent ch (GraphUpdate metric newGraph)
    where
      fetchGraph = do
        App.GraphiteConfig {..} <- view (App.config . App.graphiteConfig)
        datapoints <- getMetrics $ RenderRequest _fromTime _toTime _targetArg
        return (toGraph datapoints)
        where
          toGraph = Graph.mkGraph . fmap Graph.extract

      inThread = hoist liftIO . void . fork

  updateGraph newGraph = do
    lift $ Brick.invalidateCacheEntry GraphView
    views (App.config . App.timezone) $ graphDisplayWidget (App.Present newGraph)
