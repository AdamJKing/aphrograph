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
  , AppComponent(..)
  , emptyState
  , AppError(..)
  , App
  , runApp
  , AppLike
  , graphData
  , timezone
  , failure
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


data AppState = AppState (Graph Time Value) TimeZone | FailedAppState AppError deriving Show

instance Eq AppState where
  (AppState graphA tzA) == (AppState graphB tzB) =
    (graphA == graphB) && (tzA == tzB)
  _ == _ = False

graphData :: AppState -> Maybe (Graph Time Value)
graphData (AppState graph _) = Just graph
graphData _                  = Nothing

timezone :: AppState -> Maybe TimeZone
timezone (AppState _ tz) = Just tz
timezone _               = Nothing

failure :: AppState -> Maybe AppError
failure (FailedAppState err) = Just err
failure _                    = Nothing

emptyState :: IO AppState
emptyState = getCurrentTimeZone <&> AppState mempty

data AppError = forall e. Exception e => AppError e

instance Exception AppError

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

instance MonadFail ( App e ) where
  fail = liftIO . Prelude.fail

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
