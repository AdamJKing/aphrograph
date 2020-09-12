module EventsSpec
  ( spec,
  )
where

import App.Components
  ( GraphDisplayWidget (NoDataDisplayWidget),
    MetricsBrowserWidget (ClosedMetricsBrowser, OpenMetricsBrowser),
  )
import qualified App.State as App
import ArbitraryInstances ()
import Brick (BrickEvent)
import qualified Brick as Brick
import CommonProperties (EventOutcome (Continue), TestM)
import ComponentMock (mockClosedMetricsBrowser)
import Control.Lens ((^?))
import Events (handleEvent, runEventHandler)
import Events.Types (AppEvent)
import qualified Graphics.Vty as Vty
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (property, (.&&.), (===))

spec :: Spec
spec = describe "Metrics Browser Events" $ do
  prop "pressing (m) opens the browser when closed" $
    let inputEvent :: Brick.BrickEvent n AppEvent
        inputEvent = Brick.VtyEvent (Vty.EvKey (Vty.KChar 'm') [])

        inputState :: App.CurrentState TestM
        inputState =
          App.Active
            ( App.ActiveState
                { _metricsView = mockClosedMetricsBrowser,
                  _graphData = NoDataDisplayWidget
                }
            )
     in runEventHandler inputState $ do
          (outcome, result') <- handleEvent inputEvent inputState

          let didContinue = outcome === Continue
          let isOpen =
                property $
                  result' ^? (App.active . App.metricsView)
                    <&> \case
                      OpenMetricsBrowser {} -> True
                      _ -> False

          return (didContinue .&&. isOpen)

  prop "pressing (m) closes the browser when open" $
    let inputEvent :: Brick.BrickEvent n AppEvent
        inputEvent = Brick.VtyEvent (Vty.EvKey (Vty.KChar 'm') [])

        inputState :: App.CurrentState TestM
        inputState =
          App.Active
            ( App.ActiveState
                { _metricsView = mockClosedMetricsBrowser,
                  _graphData = NoDataDisplayWidget
                }
            )
     in runEventHandler inputState $ do
          (outcome, result') <- handleEvent inputEvent inputState

          let didContinue = outcome === Continue
          let isClosed =
                property $
                  result' ^? (App.active . App.metricsView)
                    <&> \case
                      ClosedMetricsBrowser {} -> True
                      _ -> False

          return (didContinue .&&. isClosed)

-- describe "pressing Enter in metrics view" $
--   prop "selects the metric under the cursor as the current metric" $
--     do
--       let inputEvent = Brick.VtyEvent (Vty.EvKey Vty.KEnter [])

--       inputState <- do
--         metrics <- arbitrary `suchThat` (not . null)
--         index <- choose (0, length metrics)
--         return $
--           App.Active
--             ( App.ActiveState
--                 { _metricsView = mockOpenMetricsBrowser metrics index,
--                   _graphData = NoDataDisplayWidget
--                 }
--             )

--       runEventHandler inputState $ do
--         (outcome, result') <- handleEvent inputEvent inputState

--         let didContinue = outcome === Continue
