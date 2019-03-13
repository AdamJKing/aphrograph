{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import qualified App.Args                      as App
import qualified Data.Text.Prettyprint.Doc     as Doc
import qualified System.Environment            as Env
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
import           Graphite
import           Display.Graph.GraphBuilder
import           Display.Graph                 as Graph
import           Control.Lens                   ( view )


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

newtype AppT m a = AppT  (  (ReaderT App.Args (LoggingT Text m)) a )
  deriving (Functor, Applicative, Monad, MonadLog Text, MonadReader App.Args, MonadIO)

instance MonadTrans AppT where
  lift op = AppT (lift (lift op))

runAppT :: Handler m Text -> App.Args -> AppT m a -> m a
runAppT logger args (AppT op) = runLoggingT (runReaderT op args) logger

mkApp :: Handler IO Text -> App.Args -> App AppState AppEvent AppComponent
mkApp logger args = App
  { appDraw         = return . graphWidget . graphData
  , appChooseCursor = Brick.neverShowCursor
  , appHandleEvent  = let logEventM = liftIO . logger
                      in  \currentState event -> runAppT logEventM args $ do
                            handledEvent <- runGraphiteT $ runLoggingT
                              ( usingReaderT args
                              $ appEventHandler event currentState
                              )
                              (liftIO . logger)
                            case handledEvent of
                              Continue newState ->
                                lift $ Brick.continue newState
                              Stop -> lift $ Brick.halt currentState
  , appStartEvent   =
    let logEventM = liftIO . logger
    in
      \_ -> runAppT logEventM args $ runGraphiteT $ runLoggingT
        (usingReaderT args $ do
          time   <- view App.timeArg
          target <- view App.targetArg
          data'  <- getMetricsForPast target time
          let newState = AppState (Graph.mkGraph (Graph.extract <$> data'))
          return newState
        )
        (liftIO . logger)
  , appAttrMap      = \_ -> attrMap defAttr []
  }

