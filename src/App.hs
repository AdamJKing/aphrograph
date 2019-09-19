{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}

module App where

import           Data.HList.ContainsType
import           Fmt
import           Control.Monad.Trans.MultiReader.Lazy
import           Control.Lens
import           Control.Monad.Except    hiding ( runExceptT )
import           Data.Time.LocalTime
import           Display.Graph
import           Network.HTTP.Req              as Req
import           Graphite.Types
import           Control.Monad.Log             as Log
import           Text.Show.Functions            ( )
import           App.Config

newtype AppError = AppGraphiteError GraphiteError
  deriving ( Show, Exception , Generic )

type AppLogger m = Log.Handler m [Text]

data ActiveState = ActiveState {
     _metricsView :: Maybe [Metric]
   , _graphData :: Graph Time Value
   , _timezone ::  TimeZone
} deriving ( Show , Generic, Eq )

makeLenses ''ActiveState

constructDefaultContext :: AppConfig -> IO AppState
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

type AppDependencies m = '[AppConfig, AppLogger m]

newtype AppT m a = AppT ( MultiReaderT ( AppDependencies  m) m a )
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans AppT where
  lift = AppT . lift

instance {-# OVERLAPPING #-} (ContainsType a ( AppDependencies m ),  Monad m ) => MonadMultiReader a (AppT m) where
  mAsk = AppT mAsk

instance Monad m => MonadLog Fmt.Builder (AppT m)  where
  logMessageFree addLog = mAsk @(AppLogger m) >>= lift . ($ addLog (one . fmt))


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

-- data AppF r m where
--   MkAppDraw ::l -> c -> (s -> [Brick.Widget n] )
--   MkAppChooseCursor ::l -> c -> (s -> [Brick.CursorLocation n] -> Maybe (Brick.CursorLocation n))
--   MkAppHandleEvent ::l -> c -> (s -> BrickEvent n e -> Brick.EventM n (Brick.Next s))
--   MkAppStateEvent ::l -> c (s -> Brick.EventM n s )
--   MkAppAttrMap ::AppT c (s -> Brick.AttrMap )

