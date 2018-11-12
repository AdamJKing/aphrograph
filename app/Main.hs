{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import           App
import           Events
import           Display.Graph                 as Graph
import           Display.Widgets
import qualified Graphics.Vty                  as Vty
import           Brick.AttrMap
import           Graphics.Vty.Attributes

import           Control.Monad                  ( void
                                                , forever
                                                )
import           Control.Concurrent             ( threadDelay
                                                , forkIO
                                                )
import           Brick.Main                    as Brick
import           System.IO                      ( withFile
                                                , IOMode(WriteMode)
                                                )
import           Control.Monad.Log
import qualified Args
import           Data.Text.Prettyprint.Doc
import qualified Brick.BChan                   as Brick
import qualified System.Environment            as Env

main :: IO ()
main = do
  eventQueue <- Brick.newBChan 10
  maybeArgs  <- Args.parseAppArgs <$> Env.getArgs

  case maybeArgs of
    Left  err  -> putStrLn err
    Right args -> do
      let getVty = Vty.userConfig >>= Vty.mkVty

      withFile "aphrograph.log" WriteMode $ \logfile ->
        withFDHandler defaultBatchingOptions logfile 0.4 80 $ \handler -> do
          _ <- forkIO . forever $ do
            Brick.writeBChan eventQueue GraphRefresh
            threadDelay 30000000

          runLoggingT (logMessage . pretty $ show args) handler
          void $ Brick.customMain getVty
                                  (Just eventQueue)
                                  (mkApp handler args)
                                  NoData

mkApp
  :: Handler IO (Doc String)
  -> Args.AppArgs
  -> App (Graph Integer Integer) GraphRefreshEvent Components
mkApp handler args = App
  { appDraw         = return . graphWidget
  , appChooseCursor = \_ -> const Nothing
  , appHandleEvent  = eventHandler handler args
  , appStartEvent   = return
  , appAttrMap      = \_ -> attrMap defAttr []
  }
