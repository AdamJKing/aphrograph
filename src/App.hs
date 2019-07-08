{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
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
  , AppT
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
import qualified Text.Show                     as TS
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

newtype AppT e m a =
    AppT { _unApp :: (ExceptT e (ReaderT App.Args (LoggingT Text m))) a }
    deriving ( Functor, Applicative, Monad, MonadLog Text, MonadReader App.Args
             , MonadIO, MonadError e)

instance MonadIO m => AppLike (AppT AppError m)

instance MonadTrans ( AppT e ) where
  lift = AppT . lift . lift . lift

adaptError :: Functor m => AppT e m a -> (e -> e') -> AppT e' m a
adaptError (AppT op) f = AppT (f `withExceptT` op)

runApp :: Monad m => Handler m Text -> App.Args -> AppT AppError m a -> m a
runApp logger args = runApp' >=> \case
  Right result -> return result
  Left  err    -> error (show err)
 where
  runApp' = (`runLoggingT` logger) . usingReaderT args . runExceptT . _unApp

instance MonadIO m => MonadHttp (AppT GraphiteError m) where
  handleHttpException err = throwError $ HttpError err

instance MonadIO m => MonadGraphite ( AppT AppError m) where
  getMetrics request = view graphiteUrl
    >>= \url -> getMetricsHttp url request `adaptError` AppError
