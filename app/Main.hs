{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import           Events
import           Control.Monad                  ( void )
import qualified Graphics.Vty                  as Vty
import           Brick.AttrMap
import           Graphics.Vty.Attributes

import           Display
import           Brick.Main                    as Brick
import           System.IO                      ( withFile
                                                , IOMode(WriteMode)
                                                )
import           Control.Monad.Log
import           Data.Text                      ( pack )
import           Graphite                       ( DataPoint
                                                , getMetricsForPast
                                                )
import qualified Args
import           Data.Text.Prettyprint.Doc      ( Doc )
import qualified Brick.BChan                   as Brick


main :: IO ()
main = do
  eventQueue <- Brick.newBChan 10
  args       <- Args.getAppArgs
  let getVty = Vty.userConfig >>= Vty.mkVty
  withFile "aphrograph.log" WriteMode
    $ \logfile ->
        withFDHandler defaultBatchingOptions logfile 0.4 80 $ \handler -> do
          points <- runLoggingT
            (getMetricsForPast (pack (Args._target args)) (Args._time args))
            handler
          void
            $ Brick.customMain getVty (Just eventQueue) (mkApp handler) points

mkApp :: Handler IO (Doc String) -> App [DataPoint] LoggingEvent ()
mkApp handler = App
  { appDraw         = return . renderGraphWidget
  , appChooseCursor = \_ -> const Nothing
  , appHandleEvent  = loggedEventHandler handler
  , appStartEvent   = return
  , appAttrMap      = \_ -> attrMap defAttr []
  }

