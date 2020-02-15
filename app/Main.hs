{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
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
import Control.Lens.Getter
import Control.Monad (void)
import Control.Monad.Log
import qualified Data.Text.Prettyprint.Doc as Doc
import Display.Widgets
import Events
import qualified Graphics.Vty as Vty
import Prelude hiding (on)

main :: IO ()
main = do
  eventQueue <- Brick.newBChan 10
  void $ App.withCommandLineArguments $ \args -> withFile "aphrograph.log" WriteMode $ \logfile ->
    withFDHandler defaultBatchingOptions logfile 0.4 80 $ \handler -> do
      _ <- forkIO . forever $ do
        threadDelay 30000000
        Brick.writeBChan eventQueue UpdateEvent
      startState <- App.constructDefaultContext args
      initialVty <- getVty
      let app = mkApp (liftIO . prettier handler) args
      Brick.writeBChan eventQueue UpdateEvent
      Brick.customMain
        initialVty
        getVty
        (Just eventQueue)
        app
        ( App.CurrentState
            (Right startState)
            (DisplayWidget (Right (DefaultDisplay NoDataDisplayWidget Nothing)))
        )
  where
    prettier f = f . Doc.pretty

getVty :: MonadIO m => m Vty.Vty
getVty = liftIO (Vty.userConfig >>= Vty.mkVty)

appTheme :: Brick.AttrMap
appTheme =
  let selectedTheme = ("metric" <> "selected", Vty.black `on` Vty.blue)
      unselectedTheme = ("metric" <> "unselected", Vty.blue `on` Vty.black)
   in Brick.attrMap Vty.defAttr [selectedTheme, unselectedTheme]

appDraw :: App.CurrentState -> [Brick.Widget AppComponent]
appDraw = views App.renderData compileLayered

appChooseCursor ::
  App.CurrentState ->
  [Brick.CursorLocation AppComponent] ->
  Maybe (Brick.CursorLocation AppComponent)
appChooseCursor = Brick.neverShowCursor

mkAppHandleEvent ::
  App.Logger (Brick.EventM AppComponent) ->
  App.Config ->
  App.CurrentState ->
  Brick.BrickEvent AppComponent AppEvent ->
  Brick.EventM AppComponent (Brick.Next App.CurrentState)
mkAppHandleEvent logger conf s e = runApp logger conf $ handleBrickEvent e s

appStartEvent :: App.CurrentState -> Brick.EventM AppComponent App.CurrentState
appStartEvent = return

appAttrMap :: App.CurrentState -> Brick.AttrMap
appAttrMap = const appTheme

mkApp ::
  Logger (Brick.EventM AppComponent) ->
  App.Config ->
  Brick.App App.CurrentState AppEvent AppComponent
mkApp logger conf =
  let appHandleEvent = mkAppHandleEvent logger conf
   in (Brick.App {appDraw, appChooseCursor, appHandleEvent, appAttrMap, appStartEvent})
