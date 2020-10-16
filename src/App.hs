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
import Control.Monad.Base (MonadBase, liftBase)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Logger (LogLine, LoggingT, MonadLogger, logInfo, mapLoggingT, runChanLoggingT)
import Control.Monad.Morph (MFunctor (..))
import Control.Monad.Trans.Control (MonadBaseControl (..))
import Display.GraphWidget (graphDisplayWidget)
import Events.Types
  ( AppEvent,
    MonadEvent (..),
  )
import Formatting (int, sformat, (%))
import Graphite (runGraphite)
import Graphite.Types as Graphite (MonadGraphite (..))

newtype AppChan e = AppChan (Brick.BChan e)

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
      MonadLogger
    )

instance Monad m => MonadBase m (AppT m) where
  liftBase = lift
  {-# INLINEABLE liftBase #-}

deriving instance (MonadBase b (AppT m), MonadBaseControl b m) => MonadBaseControl b (AppT m)

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

instance GraphViewer (AppT (Brick.EventM ComponentName)) where
  updateGraph ctx newGraph = do
    lift $ Brick.invalidateCacheEntry GraphView
    views (App.config . App.timezone) $ graphDisplayWidget ctx newGraph

instance MonadEvent AppEvent (AppT IO) where
  writeEvent ev = do
    (AppChan ch) <- view App.eventCh
    liftBase (Brick.writeBChan ch ev)

  writeEventLater eventAction = void $ fork (eventAction >>= writeEvent)

type AppM = AppT (Brick.EventM ComponentName)
