{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Events.Types where

import App.Components
import qualified Brick.Types as Brick
import qualified Brick.Widgets.List as BWL
import Display.Graph
import Graphite.Types

data AppEvent = TriggerUpdate | GraphUpdate (Graph Time Value)
  deriving (Show, Eq)

type SystemEvent = Brick.BrickEvent AppComponent AppEvent

type MetricsView = BWL.List AppComponent Metric

class Monad m => GraphViewer m where
  updateGraph :: m (Graph Time Value)

class Monad m => MonadOutcome (m :: Type -> Type) where
  type EventF (m :: Type -> Type) :: Type -> Type

  continue :: a -> m ((EventF m) a)
  stop :: a -> m ((EventF m) a)

class MonadOutcome m => MonadEventHandler e a m where
  handleEvent :: e -> a -> m (EventF m a)

class MonadEvent ch m where
  writeEvent :: ch e -> e -> m ()
