{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Events.Types where

import Display.Graph (Graph)
import Graphite.Types as Graphite (Time, Value)

data AppEvent = TriggerUpdate | GraphUpdate (Graph Time Value)
  deriving (Show, Eq)

data family Outcome (m :: * -> *) s

class Monad m => MonadEvent e m where
  writeEvent :: e -> m ()
  writeEventLater :: m e -> m ()
