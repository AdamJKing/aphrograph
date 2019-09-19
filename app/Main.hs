{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import           Control.Monad.Trans.MultiReader.Lazy
import           App
import qualified App.Args                      as App
import           App.Config
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
import           Data.HList.HList

main :: IO ()
main = do
  eventQueue <- Brick.newBChan 10
  void $ App.withCommandLineArguments $ \args -> withFile "aphrograph.log" WriteMode $ \logfile ->
    withFDHandler defaultBatchingOptions logfile 0.4 80 $ \handler -> do
      _ <- forkIO . forever $ do
        threadDelay 30000000
        Brick.writeBChan eventQueue UpdateEvent
      startState <- constructDefaultContext args
      initialVty <- getVty
      let deps = prettier handler :+: args :+: HNil
      app <- runMultiReaderTNil $ withMultiReaders deps mkApp
      Brick.customMain initialVty getVty (Just eventQueue) app startState
  where prettier f = f . Doc.pretty . fmap toString

getVty :: MonadIO m => m Vty.Vty
getVty = liftIO (Vty.userConfig >>= Vty.mkVty)


class WithEnv r where
  withEnvironment' :: (Monad (t m)) => (r -> m a) -> (t m) (r -> m a)
  withEnvironment' = return

instance WithEnv (r -> r' -> m a) where
  withEnvironment' = return . ($)

-- f :: ??? m => a -> m b


mkApp :: MultiReaderT '[AppLogger IO, AppConfig] IO (Brick.App AppState AppEvent AppComponent)
mkApp = do
  let appDraw         = compileLayered . constructDom
  let appChooseCursor = Brick.neverShowCursor
  appHandleEvent <- withEnvironment' handleEvent
  return (Brick.App { .. })

  -- Brick.App
  -- { appDraw         = compileLayered . constructDom
  -- , appChooseCursor = Brick.neverShowCursor
  -- , appHandleEvent  =   -- , appStartEvent   = \case
  --                       Active st -> liftIO . runAppM st $ do
  --                         newGraphData <- updateGraphData
  --                         logMessage "Producing new state."
  --                         return (Active (st & graphData .~ newGraphData))
  --                       Failed err -> return (Failed err)
  -- , appAttrMap      =
  --   \_ -> attrMap defAttr [("metric" <> "selected", black `on` blue), ("metric" <> "unselected", blue `on` black)]
  -- }
