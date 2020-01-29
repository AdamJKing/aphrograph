{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module App where

import qualified App.Config                    as App
import qualified App.State                     as App
import           Fmt
import           Control.Monad.Log             as Log
import           Text.Show.Functions            ( )
import           Control.Monad.Except           ( MonadError
                                                , throwError
                                                , catchError
                                                )
import           Control.Lens.Getter
import           Control.Lens.Operators  hiding ( (??) )
import           Control.Lens.Prism             ( _Just )
import           Control.Lens.Combinators       ( traverseOf )
import           App.Components
import           Display.GraphWidget
import           Events
import qualified Brick.Main                    as Brick
import qualified Brick.Types                   as Brick
import           Graphite
import qualified Display.Graph                 as Graph
import           Graphite.Types

type Logger m = Log.Handler m Text

newtype AppT m a = MkAppT { _unApp :: ReaderT App.Config (  ExceptT App.Error (LoggingT Fmt.Builder m ) ) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadLog Fmt.Builder
           , MonadReader App.Config
           , MonadError App.Error
           )

instance MonadTrans AppT where
  lift = MkAppT . lift . lift . lift

instance Monad m => App.Configured App.Config (AppT m) where
  getConfig = view
  {-# INLINE getConfig #-}

runApp :: Monad m => Logger m -> App.Config -> AppT m a -> m a
runApp logger conf action =
  fmap convertToRuntimeError $ runLoggingT ?? (logger . fmt) $ runExceptT $ usingReaderT conf $ _unApp action
  where convertToRuntimeError = either (error . ("Unhandled app error: " <>) . toText . displayException) id

constructDom :: App.CurrentState -> DisplayWidget App.Error
constructDom (App.Failed (App.FailedState err)) = DisplayWidget $ Left (ErrorWidget err)
constructDom (App.Active activeState          ) = DisplayWidget $ Right $ DefaultDisplay
  { dataDisplay   = graphDisplayWidget (activeState ^. App.graphData) (activeState ^. App.timezone)
  , metricBrowser = activeState ^? toMetricBrowser
  }
  where toMetricBrowser = App.metricsView . _Just . to MetricsBrowser

instance MonadIO m => MonadGraphite (AppT m) where
  getMetrics req = do
    conf     <- view App.graphiteConfig
    response <- runGraphite conf (getMetrics req)
    case response of
      Right metrics -> return metrics
      Left  err     -> throwError (App.AppGraphiteError err)

  listMetrics = do
    conf     <- view App.graphiteConfig
    response <- runGraphite conf listMetrics
    case response of
      Right metrics -> return metrics
      Left  err     -> throwError (App.AppGraphiteError err)

instance MonadOutcome Brick.Next (AppT (Brick.EventM n)) where
  continue = lift . Brick.continue
  stop     = lift . Brick.halt

type instance EventF (AppT (Brick.EventM n)) = Brick.Next

instance MonadIO m => GraphViewer (AppT m) where
  updateGraph = do
    fromTime   <- App.getConfig (App.graphiteConfig . App.fromTime)
    toTime     <- App.getConfig (App.graphiteConfig . App.toTime)
    target     <- App.getConfig (App.graphiteConfig . App.targetArg)

    datapoints <- getMetrics $ RenderRequest fromTime toTime target
    return (Graph.mkGraph $ Graph.extract <$> datapoints)

instance MonadEventHandler AppEvent (AppT (Brick.EventM n)) where
  type EventS (AppT (Brick.EventM n)) = App.CurrentState

  handleEvent UpdateEvent s = do
    newState <- App.updateGraph updateGraph s `catchError` (return . (App.failed #) . App.FailedState)
    continue newState
