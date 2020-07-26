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
import qualified App.Config as App
import qualified App.State as App
import qualified Brick.BChan as Brick
import qualified Brick.Main as Brick
import qualified Brick.Types as Brick
import qualified Brick.Widgets.List as BWL
import Control.Concurrent (Chan)
import Control.Lens.Combinators
import Control.Lens.Getter
import Control.Monad.Base
import Control.Monad.Except
import Control.Monad.Logger (LogLine, LoggingT, MonadLogger, logInfo, mapLoggingT, runChanLoggingT)
import Control.Monad.Morph
import Control.Monad.Trans.Control
import qualified Display.Graph as Graph
import Display.GraphWidget
import Events.Types
import Formatting (int, sformat, (%))
import Graphite
import Graphite.Types
import Relude

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

constructDom :: App.CurrentState -> DisplayWidget App.Error
constructDom (App.Failed (App.FailedState err)) = DisplayWidget $ Left (ErrorWidget err)
constructDom (App.Active activeState) =
  DisplayWidget $
    Right $
      DefaultDisplay
        { dataDisplay = graphDisplayWidget (activeState ^. App.graphData) (activeState ^. App.timezone),
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

instance MonadIO m => GraphViewer (AppT m) where
  updateGraph = do
    App.GraphiteConfig {..} <- view (App.config . App.graphiteConfig)
    datapoints <- getMetrics $ RenderRequest _fromTime _toTime _targetArg
    return (toGraph datapoints)
    where
      toGraph = Graph.mkGraph . fmap Graph.extract

instance MetricsBrowser (AppT (Brick.EventM AppComponent)) where
  updateMetricBrowserWidget keyPress (MetricsBrowser browser width) = lift $ do
    adjustedList <- BWL.handleListEventVi BWL.handleListEvent keyPress browser
    return (MetricsBrowser adjustedList width)
