{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import           App

import           Events

import           Display.Types
import           Display.Widgets
import qualified Graphics.Vty                  as Vty
import           Brick.AttrMap
import           Graphics.Vty.Attributes

import           Control.Monad                  ( void )
import           Control.Concurrent             ( threadDelay
                                                , forkIO
                                                )
import           Brick.Main                    as Brick
import           Control.Monad.Log
import qualified Args
import           Data.Text.Prettyprint.Doc
import qualified Brick.BChan                   as Brick
import qualified Brick.Widgets.Core            as Brick
import qualified System.Environment            as Env

main :: IO ()
main = do
  eventQueue <- Brick.newBChan 10
  maybeArgs  <- Args.parseAppArgs . fmap toText <$> Env.getArgs

  case maybeArgs of
    Left  err  -> putStrLn $ toString err
    Right args -> do
      let getVty = Vty.userConfig >>= Vty.mkVty

      withFile "aphrograph.log" WriteMode $ \logfile ->
        withFDHandler defaultBatchingOptions logfile 0.4 80 $ \handler -> do
          _ <- forkIO . forever $ do
            Brick.writeBChan eventQueue GraphRefresh
            threadDelay 30000000

          let handler' = handler . pretty . toString
          void $ Brick.customMain getVty
                                  (Just eventQueue)
                                  (mkApp handler' args)
                                  emptyState

mkApp :: Handler IO Text -> Args.AppArgs -> App AppState AppEvent AppComponent
mkApp handler args = App
  { appDraw         =
    \appState ->
      [ Brick.vBox
          [graphWidget (ui_appData appState), horizontalAxisWidget appState]
      ]
  , appChooseCursor = \_ -> const Nothing
  , appHandleEvent  = \appState e ->
                        let handle = mkEventHandler args
                        in  runLoggingT (handle appState e) (liftIO . handler)
  , appStartEvent   = return
  , appAttrMap      = \_ -> attrMap defAttr []
  }
