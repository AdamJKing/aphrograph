{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import           App
import qualified App.Args                      as App
import           Control.Lens
import           Brick.AttrMap
import           Brick.Util                     ( on )
import           Prelude                 hiding ( on )
import qualified Brick.BChan                   as Brick
import qualified Brick.Types                   as Brick
import qualified Brick.Main                    as Brick
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
import           Graphics.Vty.Attributes

main :: IO ()
main = do
  eventQueue <- Brick.newBChan 10
  void $ App.withCommandLineArguments $ \args ->
    withFile "aphrograph.log" WriteMode $ \logfile ->
      withFDHandler defaultBatchingOptions logfile 0.4 80 $ \handler -> do
        _ <- forkIO . forever $ do
          threadDelay 30000000
          Brick.writeBChan eventQueue UpdateEvent
        startState <- constructDefaultContext (prettier handler) args
        initialVty <- getVty
        Brick.customMain initialVty getVty (Just eventQueue) mkApp startState
  where prettier f = f . Doc.pretty . toString

getVty :: MonadIO m => m Vty.Vty
getVty = liftIO (Vty.userConfig >>= Vty.mkVty)

mkApp :: Brick.App AppState AppEvent AppComponent
mkApp = Brick.App
  { appDraw         = compileLayered . constructDom
  , appChooseCursor = Brick.neverShowCursor
  , appHandleEvent  = \state' event -> case state' of
                        Active st  -> handleAppEvent st event
                        Failed err -> Brick.halt (Failed err)
  , appStartEvent   = \case
                        Active st -> liftIO . runAppM st $ do
                          newGraphData <- updateGraphData
                          logMessage "Producing new state."
                          return (Active (st & graphData .~ newGraphData))
                        Failed err -> return (Failed err)
  , appAttrMap      = \_ -> attrMap
                        defAttr
                        [ ("metric" <> "selected"  , black `on` blue)
                        , ("metric" <> "unselected", blue `on` black)
                        ]
  }

handleAppEvent
  :: ActiveState
  -> SystemEvent AppComponent
  -> Brick.EventM AppComponent (Brick.Next AppState)
handleAppEvent currentState event = handleResult
  =<< liftIO (runAppM currentState (appEventHandler event))

 where
  handleResult = \case
    (Continue (Failed err)) -> Brick.halt (Failed err)
    (Continue sameState   ) -> Brick.continue sameState
    (Update   newState    ) -> Brick.continue newState
    Stop                    -> Brick.halt (Active currentState)
