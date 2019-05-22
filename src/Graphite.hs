{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Graphite where

import           Control.Monad.Except

import           Data.Aeson                    as JSON
import           Data.Aeson.Types              as JSON
import qualified Data.Vector                   as V

import           Graphite.Types

import           Network.HTTP.Req              as Req

class (Monad m) => MonadGraphite m where
  getMetricsForPast :: Text -> From -> To -> m [DataPoint]

newtype GraphiteError = GraphiteError Text deriving (Eq, Show)

instance (MonadGraphite m) => MonadGraphite (ReaderT a m) where
  getMetricsForPast a b c = lift $ getMetricsForPast a b c

constructQueryParams :: Text -> From -> To -> Req.Option 'Http
constructQueryParams target (From from) (To to) = mconcat
  ["target" =: target, "from" =: from, "to" =: to, "format" =: ("json" :: Text)]

parseMetricTimeSeries
  :: MonadError GraphiteError m => JSON.Value -> m [DataPoint]
parseMetricTimeSeries v = case JSON.parseEither parseDataPoints v of
  Right metrics -> return metrics
  Left  err     -> throwError (GraphiteError (fromString err))
 where
  unwrapArray arr = guard (not (V.null arr)) >> return (V.unsafeHead arr)

  parseDataPoints body = do
    unwrapped <- JSON.withArray "Array Wrapper" unwrapArray body
    JSON.withObject "Datapoints" (`JSON.parseField` "datapoints") unwrapped
