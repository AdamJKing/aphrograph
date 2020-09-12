{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import App
  ( AppChan (..),
    AppSystem' (AppSystem),
    constructDom,
    runApp,
  )
import qualified App.Args as App
import App.Components
  ( AppComponent,
    GraphDisplayWidget (NoDataDisplayWidget),
  )
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
import Display.Widgets (CompileLayeredWidget (compileLayered))
import Events (MonadEventHandler (handleEvent), runEventHandler)
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
          let startState = App.ActiveState {_metricsView = Nothing, _graphData = NoDataDisplayWidget}
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

mkApp :: Chan LogLine -> AppChan AppEvent -> App.Config -> Brick.App App.CurrentState AppEvent AppComponent
mkApp logging chan conf =
  let appDraw :: App.CurrentState -> [Brick.Widget AppComponent]
      appDraw = compileLayered . constructDom
      appChooseCursor ::
        App.CurrentState -> [Brick.CursorLocation AppComponent] -> Maybe (Brick.CursorLocation AppComponent)
      appChooseCursor = Brick.neverShowCursor
      appHandleEvent ::
        App.CurrentState ->
        Brick.BrickEvent AppComponent AppEvent ->
        Brick.EventM AppComponent (Brick.Next App.CurrentState)
      appHandleEvent s e = runApp logging (AppSystem conf chan) (runEventHandler (handleEvent e s) s)
      appStartEvent :: App.CurrentState -> Brick.EventM AppComponent App.CurrentState
      appStartEvent = return
      appAttrMap :: App.CurrentState -> Brick.AttrMap
      appAttrMap = const appTheme
   in (Brick.App {..})
