{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Events.Types where

import Display.Graph (Graph)
import qualified Graphics.Vty as Vty
import Graphite.Types as Graphite (Metric, Time, Value)

data AppEvent = TriggerUpdate | GraphUpdate Graphite.Metric (Graph Time Value)
  deriving (Show, Eq)

data MetricsBrowserEvent = Modify Vty.Event

class Monad m => MonadOutcome (m :: Type -> Type) where
  type EventF (m :: Type -> Type) :: Type -> Type

  continue :: a -> m ((EventF m) a)
  stop :: a -> m ((EventF m) a)

class MonadEvent ch m where
  writeEvent :: ch e -> e -> m ()
