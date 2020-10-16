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
  ( MetricsBrowserWidget,
  )
import Control.Lens.Combinators
  ( makeLenses,
    makePrisms,
  )
import Display.GraphWidget (GraphWidget)
import Graphite.Types as Graphite (GraphiteError)

newtype Error = AppGraphiteError GraphiteError
  deriving (Show, Generic)
  deriving anyclass (Exception)

data ActiveState (m :: * -> *) = ActiveState
  { _metricsView :: MetricsBrowserWidget m,
    _graphData :: GraphWidget
  }
  deriving (Show)

makeLenses ''ActiveState

newtype FailedState = FailedState {failure :: Error}
  deriving (Show, Generic)

data CurrentState (m :: * -> *) = Failed FailedState | Active (ActiveState m)
  deriving (Generic, Show)

makePrisms ''CurrentState
