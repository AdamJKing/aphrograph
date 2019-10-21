{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}

module Main where

import           App
import qualified App.Args                      as App
import qualified App.State                     as App
import qualified App.Config                     as App
import           Prelude                 hiding ( on )
import qualified Brick.BChan                   as Brick
import qualified Brick.Main                    as Brick
import qualified Brick.AttrMap                 as Brick
import           Brick.Util                     ( on )
import           Display
import           Control.Concurrent             ( forkIO
                                                , threadDelay
                                                )
import           Control.Monad                  ( void )
import           Control.Monad.Log
import           Display.Widgets
import qualified Data.Text.Prettyprint.Doc     as Doc
import           Events
import qualified Graphics.Vty                  as Vty

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
      Brick.customMain initialVty getVty (Just eventQueue) app startState
  where prettier f = f . Doc.pretty . fmap toString

getVty :: MonadIO m => m Vty.Vty
getVty = liftIO (Vty.userConfig >>= Vty.mkVty)

appTheme :: Brick.AttrMap
appTheme =
  let selectedTheme   = ("metric" <> "selected", Vty.black `on` Vty.blue)
      unselectedTheme = ("metric" <> "unselected", Vty.blue `on` Vty.black)
  in  Brick.attrMap Vty.defAttr [selectedTheme, unselectedTheme]

mkApp :: Logger -> App.Config -> Brick.App App.ActiveState AppEvent AppComponent
mkApp appLogger conf =
  let appDraw         = compileLayered . constructDom
      appChooseCursor = Brick.neverShowCursor
      appHandleEvent s e = runApp appLogger conf (handleBrickEvents e s)
      appAttrMap    = const appTheme
      appStartEvent = return
  in  (Brick.App { .. })
