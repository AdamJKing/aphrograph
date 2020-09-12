{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module App.State where

import App.Components
  ( GraphDisplayWidget,
    MetricsBrowserWidget,
  )
import Control.Lens.Combinators
  ( Prism',
    Wrapped (_Wrapped'),
    makeLenses,
    _Right,
  )
import Display.Graph as Graph (Graph)
import Graphite.Types (GraphiteError, Time, Value)

newtype Error = AppGraphiteError GraphiteError
  deriving (Show, Generic)
  deriving anyclass (Exception)

data GraphData = Missing | Pending | Present (Graph Time Value)
  deriving (Eq, Show, Generic)

data ActiveState m = ActiveState
  { _metricsView :: MetricsBrowserWidget m,
    _graphData :: GraphDisplayWidget
  }

makeLenses ''ActiveState

newtype FailedState = FailedState {failure :: Error}
  deriving (Show, Generic)

newtype CurrentState m = CurrentState (Either FailedState (ActiveState m))
  deriving (Generic)

instance Wrapped (CurrentState m)

active :: Prism' (CurrentState m) (ActiveState m)
active = _Wrapped' . _Right

pattern Active :: (ActiveState m) -> (CurrentState m)
pattern Active s = CurrentState (Right s)

pattern Failed :: FailedState -> (CurrentState m)
pattern Failed s = CurrentState (Left s)

{-# COMPLETE Active, Failed #-}
