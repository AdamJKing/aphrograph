{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import App
import qualified App.Args as App
import App.Components
import qualified App.Config as App
import qualified App.State as App
import qualified Brick.AttrMap as Brick
import qualified Brick.BChan as Brick
import qualified Brick.Main as Brick
import qualified Brick.Types as Brick
import Brick.Util (on)
import Control.Concurrent
  ( forkIO,
    threadDelay,
  )
import Display.Widgets
import Events
import Events.Types
import qualified Graphics.Vty as Vty
import Prelude hiding (on)

main :: IO ()
main = do
  eventQueue <- Brick.newBChan 10
  void $ App.withCommandLineArguments $
    \args ->
      do
        _ <- forkIO . forever $ do
          threadDelay 30000000
          Brick.writeBChan eventQueue TriggerUpdate
        startState <- App.constructDefaultContext eventQueue args
        initialVty <- getVty
        let app = mkApp eventQueue args
        Brick.writeBChan eventQueue TriggerUpdate
        Brick.customMain initialVty getVty (Just eventQueue) app (App.Active startState)

getVty :: MonadIO m => m Vty.Vty
getVty = liftIO (Vty.userConfig >>= Vty.mkVty)

appTheme :: Brick.AttrMap
appTheme =
  let selectedTheme = ("metric" <> "selected", Vty.black `on` Vty.blue)
      unselectedTheme = ("metric" <> "unselected", Vty.blue `on` Vty.black)
   in Brick.attrMap Vty.defAttr [selectedTheme, unselectedTheme]

mkApp :: Brick.BChan AppEvent -> App.Config -> Brick.App App.CurrentState AppEvent AppComponent
mkApp chan conf =
  let appDraw :: App.CurrentState -> [Brick.Widget AppComponent]
      appDraw = compileLayered . constructDom
      appChooseCursor ::
        App.CurrentState -> [Brick.CursorLocation AppComponent] -> Maybe (Brick.CursorLocation AppComponent)
      appChooseCursor = Brick.neverShowCursor
      appHandleEvent ::
        App.CurrentState ->
        Brick.BrickEvent AppComponent AppEvent ->
        Brick.EventM AppComponent (Brick.Next App.CurrentState)
      appHandleEvent s e = runApp (AppSystem conf chan) (runEventHandler (handleEvent e s) s)
      appStartEvent :: App.CurrentState -> Brick.EventM AppComponent App.CurrentState
      appStartEvent = return
      appAttrMap :: App.CurrentState -> Brick.AttrMap
      appAttrMap = const appTheme
   in (Brick.App {..})
