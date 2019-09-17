{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Graphite where

import           Data.Aeson                    as JSON
import           Graphite.Types
import           Network.HTTP.Req              as Req
import           Control.Monad.Log

asQueryParams :: GraphiteRequest -> Req.Option s
asQueryParams RenderRequest {..} =
  mconcat ["target" =: _target, "from" =: _from, "to" =: _to, "format" =: ("json" :: Text)]

listMetricsHttp :: MonadHttp m => GraphiteUrl -> m [Metric]
listMetricsHttp (GraphiteUrl url) = makeRequest $ url /: "metrics/index.json"

getMetricsHttp :: MonadHttp m => GraphiteUrl -> GraphiteRequest -> m [DataPoint]
getMetricsHttp (GraphiteUrl url) renderRequest =
  let parameters = asQueryParams renderRequest
      renderUrl  = url /: "render"
  in  datapoints <$> makeRequest' renderUrl parameters

makeRequest :: (MonadHttp m, JSON.FromJSON a) => Url s -> m a
makeRequest url = do
  resp <- req GET url NoReqBody jsonResponse mempty
  return (responseBody resp)

makeRequest' :: (MonadHttp m, JSON.FromJSON a) => Url s -> Req.Option s -> m a
makeRequest' url params = do
  resp <- req GET url NoReqBody jsonResponse params
  return (responseBody resp)

instance MonadIO m => MonadGraphite (ReaderT GraphiteUrl m) where
  listMetrics = ask >>= runReq defaultHttpConfig . listMetricsHttp

  getMetrics request = ask >>= \url -> runReq defaultHttpConfig (getMetricsHttp url request)

instance (IsString msg,  MonadGraphite m) => MonadGraphite (LoggingT msg m) where
  listMetrics = logMessage "Listing available metrics" >> lift listMetrics

  getMetrics request = logMessage "Requesting metrics" >> lift (getMetrics request)

instance MonadGraphite m => MonadGraphite (ReaderT r m) where
  listMetrics = lift listMetrics
  getMetrics request = lift (getMetrics request)
