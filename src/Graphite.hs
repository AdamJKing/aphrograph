{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphite where

import           Data.Aeson                    as JSON
import           Graphite.Types
import           Network.HTTP.Req              as Req
import qualified App.Config                    as App

asQueryParams :: GraphiteRequest -> Req.Option s
asQueryParams RenderRequest {..} =
  mconcat ["target" =: _target, "from" =: _from, "to" =: _to, "format" =: ("json" :: Text)]

listMetricsHttp :: MonadHttp m => GraphiteUrl -> m [Metric]
listMetricsHttp (GraphiteUrl url) = makeRequest $ url /: "metrics/index.json"

getMetricsHttp :: MonadHttp m => GraphiteUrl -> GraphiteRequest -> m [DataPoint]
getMetricsHttp (GraphiteUrl url) renderRequest =
  let parameters = asQueryParams renderRequest
      renderUrl  = url /: "render"
  in  makeRequest' renderUrl parameters <&> \case
        [MetricsResponse { datapoints }] -> datapoints
        _ -> []

makeRequest :: (MonadHttp m, JSON.FromJSON a) => Url s -> m a
makeRequest url = do
  resp <- req GET url NoReqBody jsonResponse mempty
  return (responseBody resp)

makeRequest' :: (MonadHttp m, JSON.FromJSON a) => Url s -> Req.Option s -> m a
makeRequest' url params = do
  resp <- req GET url NoReqBody jsonResponse params
  return (responseBody resp)

newtype GraphiteM a = GraphiteM ( ReaderT App.GraphiteConfig ( ExceptT GraphiteError Req.Req ) a )
  deriving ( Applicative
           , Functor
           , Monad
           , MonadIO
           , MonadReader App.GraphiteConfig
           )

runGraphite :: MonadIO m => App.GraphiteConfig -> GraphiteM a -> m (Either GraphiteError a)
runGraphite conf (GraphiteM op) = liftIO $ Req.runReq Req.defaultHttpConfig (runExceptT (usingReaderT conf op))

instance Req.MonadHttp GraphiteM where
  handleHttpException = GraphiteM . lift . lift . handleHttpException

instance MonadGraphite GraphiteM where
  getMetrics request = with App.graphiteUrl $ getMetricsHttp ?? request
  {-# INLINABLE getMetrics #-}

  listMetrics = with App.graphiteUrl listMetricsHttp
  {-# INLINABLE listMetrics #-}
