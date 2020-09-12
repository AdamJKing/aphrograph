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

import App (AppT)
import App.Components
  ( GraphDisplayWidget (LoadingDataDisplayWidget),
    GraphViewer (..),
    MetricsBrowserWidget (ClosedMetricsBrowser, OpenMetricsBrowser),
    close,
    open,
    update,
  )
import qualified App.State as App
import qualified Brick.Types as Brick
import Control.Lens (traverseOf)
import Control.Monad.Base (MonadBase)
import Control.Monad.Trans.Control
  ( MonadBaseControl,
  )
import Events.Types (AppEvent (..), MetricsBrowserEvent (Modify), MonadOutcome (..))
import qualified Graphics.Vty.Input.Events as Vty
import Graphite.Types (MonadGraphite (listMetrics))

pattern KeyDown :: Char -> Vty.Event
pattern KeyDown k = Vty.EvKey (Vty.KChar k) []

pattern ExitKey :: Vty.Event
pattern ExitKey = KeyDown 'q'

newtype EventT m a = MkEventT (ReaderT (App.CurrentState m) m a)
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadIO,
      MonadReader (App.CurrentState m)
    )

instance MonadTrans EventT where
  lift = MkEventT . lift

runEventHandler :: App.CurrentState m -> EventT m a -> m a
runEventHandler startState (MkEventT op) = usingReaderT startState op

instance MonadOutcome m => MonadOutcome (EventT m) where
  type EventF (EventT m) = EventF m
  continue = lift . continue
  stop = lift . stop

handleMetricBrowserEvents :: MonadGraphite m => Vty.Event -> MetricsBrowserWidget m -> EventT m (MetricsBrowserWidget m)
handleMetricBrowserEvents (KeyDown 'm') browser@ClosedMetricsBrowser {} = open browser <$> lift listMetrics
handleMetricBrowserEvents (KeyDown 'm') browser@OpenMetricsBrowser {} = return (close browser)
handleMetricBrowserEvents keyPress browser = lift (update browser (Modify keyPress))

class MonadOutcome m => MonadEventHandler e a m where
  handleEvent :: e -> a -> m (EventF m a)

instance (MonadGraphite m, MonadOutcome m) => MonadEventHandler Vty.Event (App.CurrentState m) (EventT m) where
  handleEvent ExitKey previousState = stop previousState
  handleEvent keyPress previousState = continue =<< traverseOf (App.active . App.metricsView) (handleMetricBrowserEvents keyPress) previousState

instance (GraphViewer m, MonadOutcome m) => MonadEventHandler AppEvent (App.CurrentState m) (EventT m) where
  handleEvent graphUpdate currentState = continue =<< traverseOf (App.active . App.graphData) (\_ -> update graphUpdate) currentState
    where
      update (GraphUpdate newGraph) = lift (updateGraph newGraph)
      update TriggerUpdate = (lift triggerUpdate) >> return LoadingDataDisplayWidget

instance (GraphViewer m, MonadOutcome m, MonadGraphite m) => MonadEventHandler (Brick.BrickEvent n AppEvent) (App.CurrentState m) (EventT m) where
  handleEvent (Brick.VtyEvent keyPress) = handleEvent keyPress
  handleEvent (Brick.AppEvent appEvent) = handleEvent appEvent
  handleEvent _ = continue

deriving instance MonadBase IO (EventT (AppT IO))

deriving instance MonadBaseControl IO (EventT (AppT IO))
