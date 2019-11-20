{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}

module App.State where

import           Control.Lens.TH
import           Control.Monad.Except           ( catchError )
import           Graphite.Types
import           Display.Graph
import           Data.Time.LocalTime
import qualified App.Config                    as App
import qualified Network.HTTP.Req              as Req

newtype Error = HttpError Req.HttpException
  deriving ( Show, Generic )
  deriving anyclass Exception

data ActiveState = ActiveState {
     _metricsView :: Maybe [Metric]
   , _graphData :: Graph Time Value
   , _timezone ::  TimeZone
} deriving ( Show , Eq, Generic )

makeLenses ''ActiveState

constructDefaultContext :: MonadIO m => App.Config -> m ActiveState
constructDefaultContext _ = do
  _timezone <- liftIO (getTimezone `catchError` defaultToUtc)
  let _metricsView = mempty
  let _graphData   = mempty
  return (ActiveState { .. })
 where
  getTimezone  = liftIO getCurrentTimeZone
  defaultToUtc = const (pure utc)
