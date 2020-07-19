{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Events where

import App
import App.Components
import qualified App.State as App
import qualified Brick.Main as Brick
import qualified Brick.Types as Brick
import Control.Concurrent.Lifted
import Control.Lens.Getter
import Control.Lens.Operators
import Control.Lens.Traversal
import Control.Monad.Base
import Control.Monad.Morph
import Control.Monad.Trans.Control
import Events.Types
import qualified Graphics.Vty.Input.Events as Vty
import Graphite.Types

pattern KeyDown :: Char -> Vty.Event
pattern KeyDown k = Vty.EvKey (Vty.KChar k) []

pattern ExitKey :: Vty.Event
pattern ExitKey = KeyDown 'q'

newtype EventT m a = MkEventT (ReaderT App.CurrentState m a)
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadIO,
      MonadReader App.CurrentState,
      MonadTrans,
      MonadTransControl
    )

runEventHandler :: EventT m a -> App.CurrentState -> m a
runEventHandler (MkEventT op) startState = usingReaderT startState op

instance MonadOutcome m => MonadOutcome (EventT m) where
  type EventF (EventT m) = EventF m
  continue = lift . continue
  stop = lift . stop

instance MFunctor EventT where
  hoist f (MkEventT action) = MkEventT $ hoist f action

type EventM = EventT (Brick.EventM AppComponent)

handleMetricBrowserEvents :: (MonadGraphite m, MetricsBrowser m) => Vty.Event -> Maybe MetricsBrowserWidget -> EventT m (Maybe MetricsBrowserWidget)
handleMetricBrowserEvents (KeyDown 'm') (Just _) = return Nothing
handleMetricBrowserEvents (KeyDown 'm') Nothing = Just . metricBrowserWidget <$> lift listMetrics
handleMetricBrowserEvents keyPress (Just browser) = lift (Just <$> updateMetricBrowserWidget keyPress browser)
handleMetricBrowserEvents _ Nothing = return Nothing

instance (MonadGraphite m, MonadOutcome m, MetricsBrowser m) => MonadEventHandler Vty.Event App.CurrentState (EventT m) where
  handleEvent ExitKey previousState = stop previousState
  handleEvent keyPress previousState = continue =<< traverseOf (App.active . App.metricsView) (handleMetricBrowserEvents keyPress) previousState

instance MonadEventHandler AppEvent App.CurrentState (EventT (AppT (Brick.EventM AppComponent))) where
  handleEvent (GraphUpdate newGraph) st = do
    lift . lift $ Brick.invalidateCacheEntry GraphView
    let newState = st & (App.active . App.graphData) .~ App.Present newGraph
    continue newState
  handleEvent TriggerUpdate (App.Active previousState) = do
    triggerUpdate
    continue (App.Active (previousState & App.graphData .~ App.Pending))
  handleEvent _ previousState = continue previousState

instance MonadEventHandler (Brick.BrickEvent n AppEvent) App.CurrentState (EventT (AppT (Brick.EventM AppComponent))) where
  handleEvent (Brick.VtyEvent keyPress) = handleEvent keyPress
  handleEvent (Brick.AppEvent appEvent) = handleEvent appEvent
  handleEvent _ = continue

deriving instance MonadBase IO (EventT (AppT IO))

deriving instance MonadBaseControl IO (EventT (AppT IO))

triggerUpdate :: MonadIO m => EventT (AppT m) ()
triggerUpdate =
  viaLiftIO $
    void $
      fork $ do
        newGraph <- lift updateGraph
        lift $ do
          ch <- view App.eventCh
          writeEvent ch (GraphUpdate newGraph)
  where
    viaLiftIO = hoist (hoist liftIO)
