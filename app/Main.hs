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
import Control.Monad.Log
import qualified Data.Text.Prettyprint.Doc as Doc
import Display.Widgets
import Events
import Events.Types
import qualified Graphics.Vty as Vty
import Prelude hiding (on)

main :: IO ()
main = do
  eventQueue <- Brick.newBChan 10
  void $ App.withCommandLineArguments $ \args -> withFile "aphrograph.log" WriteMode $ \logfile ->
    withFDHandler defaultBatchingOptions logfile 0.4 80 $ \handler -> do
      _ <- forkIO . forever $ do
        threadDelay 30000000
        Brick.writeBChan eventQueue TriggerUpdate
      startState <- App.constructDefaultContext eventQueue args
      initialVty <- getVty
      let app = mkApp eventQueue _ args
      Brick.writeBChan eventQueue TriggerUpdate
      Brick.customMain initialVty getVty (Just eventQueue) app (App.Active startState)
  where
    prettier f = f . Doc.pretty

getVty :: MonadIO m => m Vty.Vty
getVty = liftIO (Vty.userConfig >>= Vty.mkVty)

appTheme :: Brick.AttrMap
appTheme =
  let selectedTheme = ("metric" <> "selected", Vty.black `on` Vty.blue)
      unselectedTheme = ("metric" <> "unselected", Vty.blue `on` Vty.black)
   in Brick.attrMap Vty.defAttr [selectedTheme, unselectedTheme]

mkApp :: Brick.BChan AppEvent -> FilePath -> App.Config -> Brick.App App.CurrentState AppEvent AppComponent
mkApp chan log conf =
  let appDraw :: App.CurrentState -> [Brick.Widget AppComponent]
      appDraw = compileLayered . constructDom
      appChooseCursor ::
        App.CurrentState -> [Brick.CursorLocation AppComponent] -> Maybe (Brick.CursorLocation AppComponent)
      appChooseCursor = Brick.neverShowCursor
      appHandleEvent ::
        App.CurrentState ->
        Brick.BrickEvent AppComponent AppEvent ->
        Brick.EventM AppComponent (Brick.Next App.CurrentState)
      appHandleEvent s e = handleBrickEvent e s
      appStartEvent :: App.CurrentState -> Brick.EventM AppComponent App.CurrentState
      appStartEvent = return
      appAttrMap :: App.CurrentState -> Brick.AttrMap
      appAttrMap = const appTheme
   in (Brick.App {..})
