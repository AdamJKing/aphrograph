{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import App (AppChan (..), AppSystem' (AppSystem), runApp)
import qualified App.Args as App
import App.Components (ComponentName, _unCompT)
import qualified App.Config as App
import App.Logging (fileWritingLogChan)
import qualified App.State as App
import qualified Brick.AttrMap as Brick
import qualified Brick.BChan as Brick
import Brick.Forms (focusedFormInputAttr)
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
import Display.Widgets (CompileLayeredWidget (compileLayered))
import Events (EventOutcome (Continue, Halt), appEventHandler, brickEventHandler, keyPressHandler)
import Events.Types (AppEvent (TriggerUpdate))
import qualified Graphics.Vty as Vty
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

          startState <- App.defaultState <$> getCurrentTimeZone

          initialVty <- getVty

          let app = mkApp logQueue (AppChan eventQueue) args
          Brick.writeBChan eventQueue TriggerUpdate
          Brick.customMain initialVty getVty (Just eventQueue) app (App.Active startState)

getVty :: MonadIO m => m Vty.Vty
getVty = liftIO (Vty.userConfig >>= Vty.mkVty)

appTheme :: Brick.AttrMap
appTheme =
  Brick.attrMap
    Vty.defAttr
    [ ("metric" <> "selected", Vty.black `on` Vty.blue),
      ("metric" <> "unselected", Vty.blue `on` Vty.black),
      (focusedFormInputAttr, Vty.black `on` Vty.blue)
    ]

mkApp :: Chan LogLine -> AppChan AppEvent -> App.Config -> Brick.App (App.CurrentState AppEvent) AppEvent ComponentName
mkApp logger appCh config = Brick.App {..}
  where
    appDraw :: App.CurrentState AppEvent -> [Brick.Widget ComponentName]
    appDraw = compileLayered

    appChooseCursor :: App.CurrentState AppEvent -> [Brick.CursorLocation ComponentName] -> Maybe (Brick.CursorLocation ComponentName)
    appChooseCursor = Brick.neverShowCursor

    appHandleEvent :: App.CurrentState AppEvent -> Brick.BrickEvent ComponentName AppEvent -> Brick.EventM ComponentName (Brick.Next (App.CurrentState AppEvent))
    appHandleEvent currentState brickEvent = do
      (outcome, result) <-
        _unCompT $
          runApp logger (AppSystem config appCh) $
            brickEventHandler keyPressHandler appEventHandler brickEvent currentState
      case outcome of
        Continue -> Brick.continue result
        Halt -> Brick.halt result

    appStartEvent :: App.CurrentState AppEvent -> Brick.EventM ComponentName (App.CurrentState AppEvent)
    appStartEvent = return

    appAttrMap :: App.CurrentState AppEvent -> Brick.AttrMap
    appAttrMap = const appTheme
