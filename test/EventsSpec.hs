{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module EventsSpec
  ( spec,
  )
where

import App.Components (close, open)
import qualified App.State as App
import ArbitraryInstances ()
import CommonProperties (FakeChanT, TestM, inMetricsView, inside, is, isClosedMetricsBrowser, isOpenMetricsBrowser, runFakeChan)
import ComponentMock (mockClosedMetricsBrowser, mockOpenMetricsBrowser)
import Control.Lens (makePrisms)
import Data.Vector ((!))
import qualified Data.Vector as Vec
import Display.GraphWidget
  ( GraphDisplay (LoadingDataDisplay, NoDataDisplay),
    GraphWidget (GraphWidget),
    graphDisplay,
    graphiteRequest,
    _graphDisplay,
    _graphiteRequest,
  )
import Events (EventOutcome (Continue, Halt), keyPressHandler)
import Events.Types (AppEvent (TriggerUpdate))
import qualified Graphics.Vty as Vty
import Graphite.Types (GraphiteRequest, requestMetric)
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Discard (Discard), Testable (property), arbitrary, counterexample, (.&&.), (===))
import Test.QuickCheck.Arbitrary.ADT (genericArbitrary)
import Test.QuickCheck.GenT (MonadGen (choose, liftGen), suchThat)
import Test.QuickCheck.Property (failed)

makePrisms ''GraphDisplay

spec :: Spec
spec = do
  describe "Metrics Browser Events" $ do
    prop "Mock browser works as expected" $
      let openable = isOpenMetricsBrowser (open (mockClosedMetricsBrowser @Identity) mempty)
          closeable = isClosedMetricsBrowser (close (mockOpenMetricsBrowser @Identity mempty 0))
       in openable .&&. closeable

    prop "pressing (m) opens the browser when closed" $
      let inputEvent :: Vty.Event
          inputEvent = Vty.EvKey (Vty.KChar 'm') []

          inputState :: GraphiteRequest -> App.CurrentState (FakeChanT AppEvent TestM)
          inputState req =
            App.Active
              ( App.ActiveState
                  { _metricsView = mockClosedMetricsBrowser,
                    _graphData = GraphWidget {_graphiteRequest = req, _graphDisplay = NoDataDisplay}
                  }
              )
       in do
            someRequest <- (liftGen genericArbitrary :: TestM GraphiteRequest)
            ((outcome, result), _) <- runFakeChan $ keyPressHandler id inputEvent (inputState someRequest)

            return $ case outcome of
              Halt -> (counterexample "App halted unexpectedly" Discard)
              Continue -> inMetricsView isOpenMetricsBrowser result

  prop "pressing (m) closes the browser when open" $
    let inputEvent :: Vty.Event
        inputEvent = Vty.EvKey (Vty.KChar 'm') []

        inputState :: GraphiteRequest -> App.CurrentState (FakeChanT AppEvent TestM)
        inputState req =
          App.Active
            ( App.ActiveState
                { _metricsView = mockOpenMetricsBrowser mempty 0,
                  _graphData = GraphWidget {_graphiteRequest = req, _graphDisplay = NoDataDisplay}
                }
            )
     in do
          someRequest <- (liftGen genericArbitrary :: TestM GraphiteRequest)
          ((outcome, result), _) <- runFakeChan $ keyPressHandler id inputEvent (inputState someRequest)

          return $ case outcome of
            Halt -> (counterexample "App halted unexpectedly" (property failed))
            Continue -> inMetricsView isClosedMetricsBrowser result

  describe "pressing Enter in metrics view" $ do
    prop "selects the metric under the cursor as the current metric when metrics are available" $
      let inputEvent :: Vty.Event
          inputEvent = Vty.EvKey Vty.KEnter []
       in do
            someRequest <- (liftGen genericArbitrary :: TestM GraphiteRequest)

            metrics <- (liftGen arbitrary) `suchThat` (not . Vec.null)
            chosen <- choose (0, length metrics - 1)

            let inputState req =
                  App.Active
                    ( App.ActiveState
                        { _metricsView = mockOpenMetricsBrowser metrics chosen,
                          _graphData = GraphWidget {_graphiteRequest = req, _graphDisplay = NoDataDisplay}
                        }
                    )

            ((outcome, result), writtenEvents) <- runFakeChan $ keyPressHandler id inputEvent (inputState someRequest)

            return $ case outcome of
              Halt -> (counterexample "App halted unexpectedly" (property failed))
              Continue ->
                let usingDesiredMetric =
                      inside
                        result
                        "Active graph data graphite request"
                        (App._Active . App.graphData . graphiteRequest)
                        (\req -> (requestMetric req) === (metrics ! chosen))

                    requestedUpdate = writtenEvents === [TriggerUpdate]
                 in inMetricsView isClosedMetricsBrowser result .&&. usingDesiredMetric .&&. requestedUpdate

    prop "when there are no metrics available, the result is the no-data-display" $
      let inputEvent :: Vty.Event
          inputEvent = Vty.EvKey Vty.KEnter []
       in do
            someRequest <- (liftGen genericArbitrary :: TestM GraphiteRequest)

            let inputState req =
                  App.Active
                    ( App.ActiveState
                        { _metricsView = mockOpenMetricsBrowser mempty 0,
                          _graphData =
                            GraphWidget
                              { _graphiteRequest = req,
                                _graphDisplay = LoadingDataDisplay
                              }
                        }
                    )

            ((outcome, result), writtenEvents) <- runFakeChan @AppEvent $ keyPressHandler id inputEvent (inputState someRequest)

            return $ case outcome of
              Halt -> (counterexample "App halted unexpectedly" (property failed))
              Continue ->
                let isNoDataDisplay =
                      inside
                        result
                        "Active graph data"
                        (App._Active . App.graphData . graphDisplay)
                        (`is` ("no data display", _NoDataDisplay))

                    noUpdate = writtenEvents === []
                 in inMetricsView isClosedMetricsBrowser result .&&. isNoDataDisplay .&&. noUpdate
