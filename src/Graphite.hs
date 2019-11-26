{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Graphite where

import           Control.Monad.Except           ( MonadError(throwError) )
import           Data.Aeson                    as JSON
import           Graphite.Types
import           Network.HTTP.Req              as Req

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

newtype GraphiteT m a = GraphiteT { _runGraphite :: ReaderT GraphiteUrl (ExceptT GraphiteError m) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError GraphiteError, MonadReader GraphiteUrl)

instance MonadTrans GraphiteT where
  lift = GraphiteT . lift . lift

instance MonadIO m => MonadGraphite (GraphiteT m) where
  listMetrics = ask >>= listMetricsHttp
  getMetrics  = (ask >>=) . (getMetricsHttp ??)

instance MonadIO m => MonadHttp (GraphiteT m) where
  handleHttpException (VanillaHttpException err   ) = throwError (HttpError err)
  handleHttpException (JsonHttpException    reason) = throwError (ParsingError (toText reason))

runGraphite :: GraphiteUrl -> GraphiteT m a -> m (Either GraphiteError a)
runGraphite url = runExceptT . usingReaderT url . _runGraphite
