{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import qualified App.Args                      as App
import qualified Data.Text.Prettyprint.Doc     as Doc
import qualified Graphics.Vty                  as Vty
import           Events
import           App
import           Graphics.Vty.Attributes
import           Control.Monad                  ( void )
import           Control.Concurrent             ( threadDelay
                                                , forkIO
                                                )
import           Brick.Main                    as Brick
import qualified Brick.BChan                   as Brick
import           Brick.AttrMap
import           Control.Monad.Log
import           Display.Graph.GraphBuilder


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
  { appDraw         = return . build graphDisplayWidget
  , appChooseCursor = Brick.neverShowCursor
  , appHandleEvent  =
    \currentState event ->
      liftIO (runApp logger args (appEventHandler event currentState))
        >>= handleEventOutcome currentState
  , appStartEvent   = \originState ->
                        handleEventOutcome' originState
                          <$> (liftIO $! runApp logger args updateGraphData)
  , appAttrMap      = \_ -> attrMap defAttr []
  }
 where
  handleEventOutcome prevState = \case
    Right (Continue newState) -> Brick.continue newState
    Right Stop                -> Brick.halt prevState
    Left  _                   -> Brick.halt prevState

  handleEventOutcome' prevState = \case
    Right newState -> newState
    Left  _        -> prevState
