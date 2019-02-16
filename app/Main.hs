{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import           App
import           Events
import qualified Graphics.Vty                  as Vty
import           Brick.AttrMap
import           Graphics.Vty.Attributes
import           Control.Monad                  ( void )
import           Control.Concurrent             ( threadDelay
                                                , forkIO
                                                )
import           Brick.Main                    as Brick
import           Control.Monad.Log
import qualified App.Args                      as App
import qualified Data.Text.Prettyprint.Doc     as Doc
import qualified Brick.BChan                   as Brick
import qualified System.Environment            as Env

main :: IO ()
main = do
  eventQueue <- Brick.newBChan 10
  maybeArgs  <- App.parseAppArgs . fmap toText <$> Env.getArgs

  case maybeArgs of
    Left  err  -> putStrLn $ toString err
    Right args -> do
      let getVty = Vty.userConfig >>= Vty.mkVty

      withFile "aphrograph.log" WriteMode $ \logfile ->
        withFDHandler defaultBatchingOptions logfile 0.4 80 $ \handler -> do
          _ <- forkIO . forever $ do
            Brick.writeBChan eventQueue UpdateEvent
            threadDelay 30000000

          let handler' = handler . Doc.pretty . toString
          void $ Brick.customMain getVty
                                  (Just eventQueue)
                                  (mkApp handler' args)
                                  emptyState

mkApp :: Handler IO Text -> App.Args -> App AppState AppEvent AppComponent
mkApp _ _ = App { appDraw         = const []
                , appChooseCursor = Brick.neverShowCursor
                , appHandleEvent  = \s _ -> Brick.continue s
                , appStartEvent   = return
                , appAttrMap      = \_ -> attrMap defAttr []
                }
