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

module App.State
  ( ActiveState (..),
    FailedState (..),
    Error (..),
    CurrentState (..),
    dialogue,
    _Active,
    graphData,
    componentState,
    defaultState,
  )
where

import App.Components
  ( ComponentName,
    ComponentState (ComponentState),
    Dialogue (Closed),
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
import Display.TimeDialogueWidget (TimeDialogueState (TimeDialogueState))
import Graphite.Types as Graphite (From (..), GraphiteError, GraphiteRequest (..))

newtype Error = AppGraphiteError GraphiteError
  deriving (Show, Generic)
  deriving anyclass (Exception)

data ActiveState e = ActiveState
  { _dialogue :: Dialogue ComponentName e,
    _graphData :: GraphWidget,
    _componentState :: ComponentState
  }

makeLenses ''ActiveState

newtype FailedState = FailedState {failure :: Error}
  deriving (Show, Generic)

data CurrentState e = Failed FailedState | Active (ActiveState e)

makePrisms ''CurrentState

defaultState :: TimeZone -> ActiveState e
defaultState userTz =
  ActiveState
    { _dialogue = Closed,
      _componentState =
        ComponentState
          { _chosenTimeOffset = TimeDialogueState TwentyFourHours,
            _chosenMetric = ""
          },
      _graphData =
        GraphWidget
          { _graphiteRequest =
              Graphite.RenderRequest
                { requestFrom = Graphite.From "-24h",
                  requestTo = Nothing,
                  requestMetric = "randomWalk(\"metric\")",
                  preferredTimeZone = userTz
                },
            _graphDisplay = NoDataDisplay
          }
    }
