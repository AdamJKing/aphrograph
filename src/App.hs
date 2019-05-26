{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExplicitForAll #-}
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

import           Control.Lens            hiding ( from
                                                , to
                                                )
import           Control.Monad.Except    hiding ( runExceptT )
import           Control.Monad.Log

import qualified Data.Aeson                    as JSON
import           Data.Time.LocalTime

import           Display.Graph

import           Fmt

import           Graphite
import           Graphite.Types

import           Network.HTTP.Req              as Req

newtype AppError = HttpError Text
    deriving ( Show, Eq )

data AppState = AppState (Graph Time Value) TimeZone | FailedAppState AppError deriving ( Show, Eq )

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
emptyState = getCurrentTimeZone <&> AppState NoData

data AppComponent = GraphView
    deriving ( Eq, Ord, Show )

class ( MonadError AppError m
      , MonadReader App.Args m
      , MonadGraphite m
      , MonadLog Text m
      ) => AppLike m

newtype App a =
    App { _unApp :: (ExceptT AppError (ReaderT App.Args (LoggingT Text IO))) a
        }
    deriving ( Functor, Applicative, Monad, MonadLog Text, MonadReader App.Args
             , MonadIO, MonadError AppError, AppLike )

instance MonadFail App where
  fail = liftIO . Prelude.fail

runApp :: Handler IO Text -> App.Args -> App a -> IO (Either AppError a)
runApp logger args =
  (`runLoggingT` logger) . usingReaderT args . runExceptT . _unApp

instance MonadHttp App where
  handleHttpException err = do
    logMessage "An issue happened."
    throwError . HttpError . fromString . displayException $ err

instance MonadGraphite App where
  getMetricsForPast target from to =
    let parameters = constructQueryParams target from to
    in  do
          logMessage "Making a call to graphite."
          url        <- views graphiteUrl $ \host -> http host /: "render"
          response   <- makeRequest url parameters
          datapoints <- runExceptT $ parseMetricTimeSeries response
          case datapoints of
            Left (GraphiteError reason) -> do
              logMessage
                (fmt $ "Error parsing metric response: " +| reason |+ ".")
              throwError $ HttpError reason
            Right dps -> logDataSize dps >> return dps
   where
    logDataSize dps =
      logMessage $ fmt "Size of returned data was: " +| length dps |+ "."

makeRequest
  :: (AppLike m, MonadHttp m)
  => JSON.FromJSON a => Url 'Http -> Req.Option 'Http -> m a
makeRequest url params = do
  resp <- req GET url NoReqBody jsonResponse params
  return (responseBody resp)
