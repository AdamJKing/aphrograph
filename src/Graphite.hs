{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphite where

import qualified App.Config as App
import Control.Lens.Getter (Getter, view)
import Control.Monad.Except (MonadError (throwError))
import Data.Aeson as JSON (FromJSON)
import Data.Vector (Vector)
import Graphite.Types
  ( DataPoint,
    GraphiteError (..),
    GraphiteRequest (..),
    GraphiteUrl (..),
    Metric,
    MetricsResponse (MetricsResponse, datapoints),
    MonadGraphite (..),
  )
import Network.HTTP.Req as Req
  ( GET (GET),
    HttpException (JsonHttpException, VanillaHttpException),
    MonadHttp (handleHttpException),
    NoReqBody (NoReqBody),
    Option,
    Req,
    Url,
    defaultHttpConfig,
    jsonResponse,
    req,
    responseBody,
    runReq,
    (/:),
    (=:),
  )

with :: MonadReader s m => Getter s a -> (a -> m b) -> m b
with lens f = view lens >>= f

asQueryParams :: GraphiteRequest -> Req.Option s
asQueryParams RenderRequest {..} =
  mconcat
    [ "target" =: toText requestMetric,
      "from" =: requestFrom,
      "to" =: requestTo,
      "format" =: ("json" :: Text),
      "tz" =: (show preferredTimeZone :: Text)
    ]

listMetricsHttp :: MonadHttp m => GraphiteUrl -> m (Vector Metric)
listMetricsHttp (GraphiteUrl url) = makeRequest $ url /: "metrics/index.json"

getMetricsHttp :: MonadHttp m => GraphiteUrl -> GraphiteRequest -> m [DataPoint]
getMetricsHttp (GraphiteUrl url) renderRequest =
  let parameters = asQueryParams renderRequest
   in makeRequest' (url /: "render") parameters <&> \case
        [MetricsResponse {datapoints}] -> datapoints
        _emptyResponse -> []

makeRequest :: (MonadHttp m, JSON.FromJSON a) => Url s -> m a
makeRequest url = do
  resp <- req GET url NoReqBody jsonResponse mempty
  return (responseBody resp)

makeRequest' :: (MonadHttp m, JSON.FromJSON a) => Url s -> Req.Option s -> m a
makeRequest' url params = do
  resp <- req GET url NoReqBody jsonResponse params
  return (responseBody resp)

newtype GraphiteM a = GraphiteM (ReaderT App.GraphiteConfig (ExceptT GraphiteError Req.Req) a)
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadIO,
      MonadReader App.GraphiteConfig,
      MonadError GraphiteError
    )

runGraphite :: MonadIO m => App.GraphiteConfig -> GraphiteM a -> m (Either GraphiteError a)
runGraphite conf (GraphiteM op) = liftIO $ Req.runReq Req.defaultHttpConfig (runExceptT (usingReaderT conf op))

instance Req.MonadHttp GraphiteM where
  handleHttpException (VanillaHttpException err) = throwError (HttpError err)
  handleHttpException (JsonHttpException err) = throwError (ParsingError (toText err))

instance MonadGraphite GraphiteM where
  getMetrics request = with App.graphiteUrl $ getMetricsHttp ?? request
  {-# INLINEABLE getMetrics #-}

  listMetrics = with App.graphiteUrl listMetricsHttp
  {-# INLINEABLE listMetrics #-}
