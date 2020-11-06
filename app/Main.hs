{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import App (AppChan (..), AppSystem' (AppSystem), runApp)
import qualified App.Args as App
import App.Components (ComponentName, _unCompT)
import qualified App.Config as App
import App.Logging (fileWritingLogChan)
import qualified App.State as App
import qualified Brick.AttrMap as Brick
import qualified Brick.BChan as Brick
import qualified Brick.Main as Brick
import qualified Brick.Types as Brick
import Brick.Util (on)
import Control.Concurrent
  ( Chan,
    forkIO,
    newChan,
    threadDelay,
  )
import Control.Monad.Logger (LogLine)
import Data.Time (getCurrentTimeZone)
import Display.GraphWidget (GraphDisplay (NoDataDisplay), GraphWidget (..))
import Display.Widgets (CompileLayeredWidget (compileLayered))
import Events (EventOutcome (Continue, Halt), appEventHandler, brickEventHandler, keyPressHandler)
import Events.Types (AppEvent (TriggerUpdate))
import qualified Graphics.Vty as Vty
import qualified Graphite.Types as Graphite
  ( From (..),
    GraphiteRequest (RenderRequest),
    preferredTimeZone,
    requestFrom,
    requestMetric,
    requestTo,
  )
import Prelude hiding (on)

main :: IO ()
main = do
  eventQueue <- Brick.newBChan 10
  logQueue <- newChan
  writeFile "aphrograph.log" ""
  fileWritingLogChan logQueue "aphrograph.log"
  void $
    App.withCommandLineArguments $
      \args ->
        do
          _ <- forkIO . forever $ do
            threadDelay 30000000
            Brick.writeBChan eventQueue TriggerUpdate

          tz <- getCurrentTimeZone

          let startState =
                App.ActiveState
                  { _metricsView = Nothing,
                    _graphData =
                      GraphWidget
                        { _graphiteRequest =
                            Graphite.RenderRequest
                              { requestFrom = (Graphite.From "-24h"),
                                requestTo = Nothing,
                                requestMetric = "randomWalk(\"metric\")",
                                preferredTimeZone = tz
                              },
                          _graphDisplay = NoDataDisplay
                        }
                  }

          initialVty <- getVty

          let app = mkApp logQueue (AppChan eventQueue) args
          Brick.writeBChan eventQueue TriggerUpdate
          Brick.customMain initialVty getVty (Just eventQueue) app (App.Active startState)

getVty :: MonadIO m => m Vty.Vty
getVty = liftIO (Vty.userConfig >>= Vty.mkVty)

appTheme :: Brick.AttrMap
appTheme =
  let selectedTheme = ("metric" <> "selected", Vty.black `on` Vty.blue)
      unselectedTheme = ("metric" <> "unselected", Vty.blue `on` Vty.black)
   in Brick.attrMap Vty.defAttr [selectedTheme, unselectedTheme]

mkApp :: Chan LogLine -> AppChan AppEvent -> App.Config -> Brick.App App.CurrentState AppEvent ComponentName
mkApp logger appCh config = Brick.App {..}
  where
    appDraw :: App.CurrentState -> [Brick.Widget ComponentName]
    appDraw = compileLayered

    appChooseCursor :: App.CurrentState -> [Brick.CursorLocation ComponentName] -> Maybe (Brick.CursorLocation ComponentName)
    appChooseCursor = Brick.neverShowCursor

    appHandleEvent :: App.CurrentState -> Brick.BrickEvent ComponentName AppEvent -> Brick.EventM ComponentName (Brick.Next App.CurrentState)
    appHandleEvent currentState brickEvent = do
      (outcome, result) <-
        _unCompT $
          runApp logger (AppSystem config appCh) $
            brickEventHandler keyPressHandler appEventHandler brickEvent currentState
      case outcome of
        Continue -> (Brick.continue result)
        Halt -> (Brick.halt result)

    appStartEvent :: App.CurrentState -> Brick.EventM ComponentName App.CurrentState
    appStartEvent = return

    appAttrMap :: App.CurrentState -> Brick.AttrMap
    appAttrMap = const appTheme
