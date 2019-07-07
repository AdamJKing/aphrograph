{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module App
  ( AppState(..)
  , MetricContext(..)
  , AppComponent(..)
  , emptyState
  , AppError(..)
  , App
  , runApp
  , AppLike
  , hasFailed
  )
where

import           App.Args                      as App
import           Control.Monad.Except    hiding ( runExceptT )
import           Data.Time.LocalTime
import           Display.Graph
import           Network.HTTP.Req              as Req
import           Graphite.Types
import           Graphite
import           Control.Monad.Log
import           Text.Show                     as TS
import           Control.Lens.Getter            ( view )


data MetricContext = MetricContext {
   graphData :: Graph Time Value,  timezone ::  TimeZone
   } deriving ( Show, Eq )

data AppState = AppState MetricContext | FailedAppState AppError deriving Show

instance Eq AppState where
  (AppState ctxt) == (AppState ctxt') = ctxt == ctxt'
  _               == _                = False

hasFailed :: AppState -> Bool
hasFailed (AppState       _) = False
hasFailed (FailedAppState _) = True

emptyState :: IO AppState
emptyState = getCurrentTimeZone <&> AppState . MetricContext mempty

data AppError = forall e. Exception e => AppError e

instance Exception AppError where
  displayException (AppError err) = displayException err

instance TS.Show AppError where
  show = displayException

data AppComponent = GraphView
    deriving ( Eq, Ord, Show )

class ( MonadError AppError m , MonadReader App.Args m , MonadLog Text m , MonadGraphite m) => AppLike m

instance AppLike (App AppError)

newtype App e a =
    App { _unApp :: (ExceptT e (ReaderT App.Args (LoggingT Text IO))) a
        }
    deriving ( Functor, Applicative, Monad, MonadLog Text, MonadReader App.Args
             , MonadIO, MonadError e)

adaptError :: App e a -> (e -> e') -> App e' a
adaptError (App op) f = App (f `withExceptT` op)

runApp
  :: Handler IO Text -> App.Args -> App AppError a -> IO (Either AppError a)
runApp logger args =
  (`runLoggingT` logger) . usingReaderT args . runExceptT . _unApp

instance MonadHttp (App GraphiteError) where
  handleHttpException err = throwError $ HttpError err

instance MonadGraphite ( App AppError ) where
  getMetrics request = view graphiteUrl
    >>= \url -> getMetricsHttp url request `adaptError` AppError
