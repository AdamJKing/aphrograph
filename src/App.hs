{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module App
  ( AppError(..)
  , AppState(..)
  , ActiveState(..)
  , AppLogger
  , metricsView
  , graphData
  , timezone
  , constructDefaultContext
  , handleHttpExceptionApp
  )
where

import           Fmt
import           Control.Monad.Trans.MultiReader.Lazy
import           App.Config
import qualified App.Args                      as App
import           Control.Lens
import           Control.Monad.Except    hiding ( runExceptT )
import           Data.Time.LocalTime
import           Display.Graph
import           Network.HTTP.Req              as Req
import           Graphite.Types
import           Control.Monad.Log             as Log
import           Text.Show.Functions            ( )

newtype AppError = AppGraphiteError GraphiteError
  deriving ( Show, Exception , Generic )

type AppLogger m = Log.Handler m [Text]

data ActiveState = ActiveState {
     _metricsView :: Maybe [Metric]
   , _graphData :: Graph Time Value
   , _timezone ::  TimeZone
} deriving ( Show , Generic, Eq )

makeLenses ''ActiveState

constructDefaultContext :: App.Args -> IO AppState
constructDefaultContext _ = do
  _timezone <- getTimezone `catchError` defaultToUtc
  let _metricsView = mempty
  let _graphData   = mempty
  return . Active $ ActiveState { .. }
 where
  getTimezone  = liftIO getCurrentTimeZone
  defaultToUtc = const (pure utc)

data AppState = Active ActiveState | Failed AppError
  deriving ( Generic, Show )

type AppDependencies = '[AppConfig, AppLogger IO, AppState]

newtype AppT a = AppT ( MultiReaderT AppDependencies IO a )
  deriving (Functor, Applicative, Monad, MonadIO, MonadMultiReader AppConfig, MonadMultiReader (AppLogger IO))

instance MonadLog Fmt.Builder AppT where
  logMessageFree addLog = mAsk @(AppLogger IO) >>= \logger -> liftIO $ logger (addLog (\msg -> [fmt msg]))

-- instance Monad m => MonadError AppError (AppT m) where
--   throwError err = state (const (Failed err))

handleHttpExceptionApp :: MonadError AppError m => HttpException -> m a
handleHttpExceptionApp = throwError . AppGraphiteError . \case
  (JsonHttpException    reason) -> ParsingError (fromString reason)
  (VanillaHttpException reason) -> HttpError reason

-- instance MonadIO m => MonadHttp (AppT m) where
--   handleHttpException = handleHttpExceptionApp

-- instance MonadGraphite (AppM m) where
--   listMetrics = view (graphiteConfig . graphiteUrl) >>= listMetricsHttp

--   getMetrics request = do
--     url <- view (graphiteConfig . graphiteUrl)
--     getMetricsHttp url request

--   AppDraw ::AppT c (s -> [Brick.Widget n] )
--   AppChooseCursor ::AppT c (s -> [Brick.CursorLocation n] -> Maybe (Brick.CursorLocation n))
--   AppHandleEvent ::AppT c (s -> BrickEvent n e -> Brick.EventM n (Brick.Next s))
--   AppStateEvent ::AppT c (s -> Brick.EventM n s )
--   AppAttrMap ::AppT c (s -> Brick.AttrMap )
--   AppAttrMap ::AppT c (s -> Brick.AttrMap )
