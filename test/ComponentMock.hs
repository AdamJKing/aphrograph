{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module ComponentMock where

import App.Components (ComponentName, MetricsBrowserWidget (..))
import qualified Brick as Brick
import Data.Vector (Vector, (!?))
import Events.Types (MetricsBrowserEvent (Modify))
import qualified Graphics.Vty as Vty
import qualified Graphite.Types as Graphite

mockClosedMetricsBrowser :: Applicative m => MetricsBrowserWidget m
mockClosedMetricsBrowser = ClosedMetricsBrowser {open}
  where
    open metrics = mockOpenMetricsBrowser metrics 0

mockOpenMetricsBrowser :: Applicative m => Vector Graphite.Metric -> Int -> MetricsBrowserWidget m
mockOpenMetricsBrowser metrics index =
  OpenMetricsBrowser {update, display, close, target}
  where
    update (Modify (Vty.EvKey keyPress [])) = pure $
      mockOpenMetricsBrowser metrics $ case keyPress of
        Vty.KUp -> index + 1
        Vty.KChar 'k' -> index + 1
        Vty.KDown -> index - 1
        Vty.KChar 'j' -> index - 1
        _ -> index
    update _ = pure (mockOpenMetricsBrowser metrics index)

    target _ = metrics !? index

    display = Brick.emptyWidget @ComponentName

    close = mockClosedMetricsBrowser
