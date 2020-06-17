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
import Control.Lens.Combinators
import Control.Lens.Getter
import Control.Monad.Base
import Control.Monad.Logger
import Control.Monad.Morph
import Control.Monad.Trans.Control
import qualified Display.Graph as Graph
import Display.GraphWidget
import Events.Types
import Graphite
import Graphite.Types
import Text.Show.Functions ()

data AppSystem = AppSystem {_config :: App.Config, _eventCh :: Brick.BChan AppEvent}

makeLenses ''AppSystem

newtype AppT m a = MkAppT {_unApp :: ReaderT AppSystem (LoggingT (ExceptT App.Error m)) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader AppSystem,
      MonadError App.Error,
      MonadBase b,
      MonadBaseControl b
    )

instance MFunctor AppT where
  hoist nat m = MkAppT $ hoist (mapLoggingT $ hoist nat) (_unApp m)

instance MonadTrans AppT where
  lift = MkAppT . lift . lift . lift

runApp :: (MonadFail m, MonadBaseControl IO m) => AppSystem -> FilePath -> AppT m a -> m a
runApp deps log = failOnError . runFileLoggingT log . usingReaderT deps . _unApp
  where
    failOnError res = runExceptT res >>= \case
      (Right a) -> return a
      (Left e) -> fail (displayException e)

constructDom :: App.CurrentState -> DisplayWidget App.Error
constructDom (App.Failed (App.FailedState err)) = DisplayWidget $ Left (ErrorWidget err)
constructDom (App.Active activeState) =
  DisplayWidget $ Right $
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
      Right metrics -> return metrics
      Left err -> throwError (App.AppGraphiteError err)

instance MonadTransControl AppT where
  type StT AppT a = StT (ReaderT AppSystem) (StT LoggingT (StT (ExceptT App.Error) a))

  liftWith t = MkAppT $ liftWith (\runr -> liftWith (\runl -> liftWith (\rune -> t $ rune . runl . runr . _unApp)))
  restoreT = MkAppT . restoreT . restoreT . restoreT

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
    return (Graph.mkGraph $ Graph.extract <$> datapoints)

instance MetricsBrowser (AppT (Brick.EventM AppComponent)) where
  updateMetricBrowserWidget keyPress (MetricsBrowser browser) = lift (MetricsBrowser <$> BWL.handleListEventVi BWL.handleListEvent keyPress browser)
