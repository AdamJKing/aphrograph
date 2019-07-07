{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import           App
import qualified App.Args                      as App
import           Brick.AttrMap
import qualified Brick.BChan                   as Brick
import           Brick.Main                    as Brick
import           Display
import           Control.Concurrent             ( forkIO
                                                , threadDelay
                                                )
import           Control.Monad                  ( void )
import           Control.Monad.Log
import           Control.Monad.Except
import           Display.Widgets
import qualified Control.Exception             as Exc
import qualified Data.Text.Prettyprint.Doc     as Doc
import           Data.Time.LocalTime
import           Events
import qualified Graphics.Vty                  as Vty
import           Graphics.Vty.Attributes
import           System.IO.Unsafe

main :: IO ()
main = do
  eventQueue <- Brick.newBChan 10
  void $ App.withCommandLineArguments $ \args ->
    withFile "aphrograph.log" WriteMode $ \logfile ->
      withFDHandler defaultBatchingOptions logfile 0.4 80 $ \handler -> do
        _ <- forkIO . forever $ do
          threadDelay 30000000
          Brick.writeBChan eventQueue UpdateEvent
        executeApp eventQueue (prettier handler) args
  where prettier f = f . Doc.pretty . toString

getVty :: IO Vty.Vty
getVty = Vty.userConfig >>= Vty.mkVty

executeApp :: Brick.BChan AppEvent -> Handler IO Text -> App.Args -> IO AppState
executeApp eq handler args = do
  initialVty <- getVty
  startState <- emptyState
  Brick.customMain initialVty getVty (Just eq) (mkApp handler args) startState

mkApp :: Handler IO Text -> App.Args -> Brick.App AppState AppEvent AppComponent
mkApp logger args = Brick.App
  { appDraw         = pure . compile . constructDom
  , appChooseCursor = Brick.neverShowCursor
  , appHandleEvent  =
    \currentState event ->
      liftIO (runApp logger args (appEventHandler event currentState))
        >>= handleEventOutcome currentState
  , appStartEvent   = liftIO . generateStartEvent
  , appAttrMap      = \_ -> attrMap defAttr []
  }
 where
  generateStartEvent :: AppState -> IO AppState
  generateStartEvent prevState = handleEventOutcome' prevState
    <$> runApp logger args (catchError startEvent $ return . FailedAppState)
   where
    startEvent = do
      gd <- updateGraphData
      tz <- liftIO getCurrentTimeZone
      return (AppState $ MetricContext gd tz)

  handleEventOutcome prevState = \case
    Right (Continue (FailedAppState err)) -> Brick.halt (FailedAppState err)
    Right (Continue newState            ) -> Brick.continue newState
    Right Stop                            -> Brick.halt prevState
    Left  _                               -> Brick.halt prevState

  handleEventOutcome' prevState = \case
    Right newState -> newState
    Left  _        -> prevState

-- warning: unsafe when debug mode is enabled!
handleWidgetLogs :: Bool -> Handler IO Text -> [Text] -> ()
handleWidgetLogs debugMode log logs =
  unsafePerformIO $ guard debugMode >> Exc.catch (mapM_ log logs) ignoreError
  where ignoreError (_ :: Exc.ErrorCall) = pass
