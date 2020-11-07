{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module App.State where

import App.Components (TimeDialogue, MetricsBrowser)
import Control.Lens.Combinators
  ( makeLenses,
    makePrisms,
  )
import Display.GraphWidget (GraphWidget)
import Graphite.Types as Graphite (GraphiteError)

newtype Error = AppGraphiteError GraphiteError
  deriving (Show, Generic)
  deriving anyclass (Exception)

data ActiveState = ActiveState
  { _dialogue :: Maybe (Either MetricsBrowser TimeDialogue),
    _graphData :: GraphWidget
  }

makeLenses ''ActiveState

newtype FailedState = FailedState {failure :: Error}
  deriving (Show, Generic)

data CurrentState = Failed FailedState | Active ActiveState

makePrisms ''CurrentState
