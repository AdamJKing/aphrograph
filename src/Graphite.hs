{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Graphite where

import           Control.Monad.Except    hiding ( runExceptT )
import           Data.Aeson                    as JSON
import           Data.Aeson.Types              as JSON
import qualified Data.Vector                   as V
import           Graphite.Types
import           Network.HTTP.Req              as Req


asQueryParams :: GraphiteRequest -> Req.Option s
asQueryParams RenderRequest {..} = mconcat
  ["target" =: target, "from" =: from, "to" =: to, "format" =: ("json" :: Text)]

parseMetricTimeSeries
  :: MonadError GraphiteError m => JSON.Value -> m [DataPoint]
parseMetricTimeSeries v = case JSON.parseEither parseDataPoints v of
  Right metrics -> return metrics
  Left  err     -> throwError (ParsingError (fromString err))
 where
  unwrapArray arr = guard (not (V.null arr)) >> return (V.unsafeHead arr)
  parseDataPoints body = do
    unwrapped <- JSON.withArray "Array Wrapper" unwrapArray body
    JSON.withObject "Datapoints" (`JSON.parseField` "datapoints") unwrapped

getMetricsHttp
  :: (MonadHttp m, MonadError GraphiteError m)
  => GraphiteUrl
  -> GraphiteRequest
  -> m [DataPoint]
getMetricsHttp (GraphiteUrl url) renderRequest =
  let parameters = asQueryParams renderRequest
      renderUrl  = url /: "render"
  in  makeRequest renderUrl parameters >>= parseMetricTimeSeries

makeRequest :: (MonadHttp m, JSON.FromJSON a) => Url s -> Req.Option s -> m a
makeRequest url params = do
  resp <- req GET url NoReqBody jsonResponse params
  return (responseBody resp)
