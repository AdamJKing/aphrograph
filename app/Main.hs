{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import           App
import qualified App.Args                      as App
import           Brick.AttrMap
import qualified Brick.BChan                   as Brick
import qualified Brick.Types                   as Brick
import           Brick.Main                    as Brick
import           Display
import           Control.Concurrent             ( forkIO
                                                , threadDelay
                                                )
import           Control.Monad                  ( void )
import           Control.Monad.Log
import           Control.Monad.Except
import           Display.Widgets
import qualified Data.Text.Prettyprint.Doc     as Doc
import           Data.Time.LocalTime
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
        executeApp eventQueue (liftIO . prettier handler) args
  where prettier f = f . Doc.pretty . toString

getVty :: IO Vty.Vty
getVty = Vty.userConfig >>= Vty.mkVty

executeApp
  :: Brick.BChan AppEvent
  -> Handler (Brick.EventM AppComponent) Text
  -> App.Args
  -> IO AppState
executeApp eq handler args = do
  initialVty <- getVty
  startState <- emptyState
  Brick.customMain initialVty getVty (Just eq) (mkApp handler args) startState

mkApp
  :: Handler (Brick.EventM AppComponent) Text
  -> App.Args
  -> Brick.App AppState AppEvent AppComponent
mkApp logger args = Brick.App
  { appDraw         = pure . compile . constructDom
  , appChooseCursor = Brick.neverShowCursor
  , appHandleEvent  = \state' event ->
                        runApp logger args (handleAppEvent state' event)
  , appStartEvent   = runApp logger args . generateStartEvent
  , appAttrMap      = \_ -> attrMap defAttr []
  }
 where
  generateStartEvent _ = (`catchError` return . FailedAppState) $ do
    gd <- updateGraphData
    tz <- liftIO getCurrentTimeZone
    return (AppState $ MetricContext gd tz)


handleAppEvent
  :: AppState
  -> SystemEvent AppComponent
  -> AppT AppError (Brick.EventM AppComponent) (Brick.Next AppState)
handleAppEvent currentState event =
  let outcome = appEventHandler event currentState
  in  (lift . handleResult) =<< outcome

 where
  handleResult :: EventOutcome AppState -> Brick.EventM n (Brick.Next AppState)
  handleResult = \case
    (Continue (FailedAppState err)) -> Brick.halt (FailedAppState err)
    (Continue newState            ) -> Brick.continue newState
    Stop                            -> Brick.halt currentState

