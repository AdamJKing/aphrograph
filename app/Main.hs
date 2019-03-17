{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import qualified App.Args                      as App
import qualified Data.Text.Prettyprint.Doc     as Doc
import qualified Graphics.Vty                  as Vty
import           Display.Widgets
import           Events
import           App
import           Graphics.Vty.Attributes
import           Control.Monad                  ( void )
import           Control.Concurrent             ( threadDelay
                                                , forkIO
                                                )
import           Brick.Main                    as Brick
import qualified Brick.BChan                   as Brick
import qualified Brick.Widgets.Core            as Brick
import           Brick.AttrMap
import           Control.Monad.Log
import           Display.Graph.GraphBuilder


main :: IO ()
main = do
  eventQueue <- Brick.newBChan 10
  App.withCommandLineArguments $ \args -> do
    let getVty = Vty.userConfig >>= Vty.mkVty

    withFile "aphrograph.log" WriteMode $ \logfile ->
      withFDHandler defaultBatchingOptions logfile 0.4 80 $ \handler -> do
        _ <- forkIO . forever $ do
          threadDelay 30000000
          Brick.writeBChan eventQueue UpdateEvent

        let handler' = handler . Doc.pretty . toString
        void $ Brick.customMain getVty
                                (Just eventQueue)
                                (mkApp handler' args)
                                emptyState

mkApp :: Handler IO Text -> App.Args -> App AppState AppEvent AppComponent
mkApp logger args = App
  { appDraw         = \s -> return $ Brick.vBox
                        [ Brick.vLimitPercent 90 $ Brick.hBox
                          [ Brick.hLimitPercent 8 $ verticalAxisWidget (graphData s)
                          , graphWidget (graphData s)
                          ]
                        , Brick.hBox
                          [ Brick.hLimitPercent 8 cornerPiece
                          , horizontalAxisWidget (graphData s)
                          ]
                        ]
  , appChooseCursor = Brick.neverShowCursor
  , appHandleEvent  = \currentState event ->
                        runApp (appEventHandler event currentState)
                          >>= handleEventOutcome currentState
  , appStartEvent   = const $ runApp updateGraphData
  , appAttrMap      = \_ -> attrMap defAttr []
  }
 where
  runApp    = runAppT logEventM args
  logEventM = liftIO . logger
  handleEventOutcome prevState = \case
    Continue newState -> Brick.continue newState
    Stop              -> Brick.halt prevState
