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

import App.Components
  ( ComponentState (ComponentState),
    Dialogue (NotOpen),
    QuickOffset (TwentyFourHours),
    _chosenMetric,
    _chosenTimeOffset,
  )
import Control.Lens.Combinators
  ( makeLenses,
    makePrisms,
  )
import Data.Time (TimeZone)
import Display.GraphWidget (GraphDisplay (NoDataDisplay), GraphWidget (..))
import Graphite.Types as Graphite (From (..), GraphiteError, GraphiteRequest (..))

newtype Error = AppGraphiteError GraphiteError
  deriving (Show, Generic)
  deriving anyclass (Exception)

data ActiveState = ActiveState
  { _dialogue :: Dialogue,
    _graphData :: GraphWidget,
    _componentState :: ComponentState
  }

makeLenses ''ActiveState

newtype FailedState = FailedState {failure :: Error}
  deriving (Show, Generic)

data CurrentState = Failed FailedState | Active ActiveState

makePrisms ''CurrentState

defaultState :: TimeZone -> ActiveState
defaultState userTz =
  ActiveState
    { _dialogue = NotOpen,
      _componentState =
        ComponentState
          { _chosenTimeOffset = TwentyFourHours,
            _chosenMetric = ""
          },
      _graphData =
        GraphWidget
          { _graphiteRequest =
              Graphite.RenderRequest
                { requestFrom = (Graphite.From "-24h"),
                  requestTo = Nothing,
                  requestMetric = "randomWalk(\"metric\")",
                  preferredTimeZone = userTz
                },
            _graphDisplay = NoDataDisplay
          }
    }
