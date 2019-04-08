{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Graphite where

import           Fmt
import           Control.Monad.Log
import           App
import           App.Args                       ( graphiteUrl )
import           Control.Lens            hiding ( from
                                                , to
                                                )
import qualified Data.Aeson                    as JSON
import qualified Data.Aeson.Types              as JSONT
import           Network.Wreq
import qualified Data.Vector                   as V
import           Graphite.Types


class (Monad m) => MonadGraphite m where
  getMetricsForPast :: Text -> From -> To -> m [DataPoint]

instance MonadGraphite App where
  getMetricsForPast target from to =
    let parameters = constructQueryParams target from to
    in  do
          logMessage "Making a call to graphite."
          url        <- views graphiteUrl buildUrl
          response   <- liftIO $ getWith parameters url
          datapoints <- handleResponse response
          logDataSize datapoints
          return datapoints
   where
    buildUrl graphiteHost = fmt $ "http://" +| graphiteHost |+ "/render"
    logDataSize dps =
      logMessage $ fmt "Size of returned data was: " +| length dps |+ "."


handleResponse
  :: (MonadFail m, MonadLog Text m) => Response LByteString -> m [DataPoint]
handleResponse resp =
  let body = view responseBody resp
  in  do
        logMessage . fmt $ "Response from graphite:\n " +|| body ||+ "\n"
        case parseMetricTimeSeries body of
          Right dps -> return dps
          Left  err -> fail $ "Unusable response from graphite. (" +| err |+ ")"


instance (MonadGraphite m) => MonadGraphite (ReaderT a m) where
  getMetricsForPast a b c = lift $ getMetricsForPast a b c

constructQueryParams :: Text -> From -> To -> Options
constructQueryParams target (From from) (To to) =
  let targetArg = ("target", target)
      fromArg   = ("from", from)
      toArg     = ("to", to)
      jsonArg   = ("format", "json")
  in  set params [targetArg, fromArg, toArg, jsonArg] defaults

parseMetricTimeSeries :: LByteString -> Either String [DataPoint]
parseMetricTimeSeries respBody = do
  strResp <- JSON.eitherDecode respBody
  JSONT.parseEither parseDataPoints strResp

 where
  unwrapArray arr = guard (not (V.null arr)) >> return (V.unsafeHead arr)
  parseDataPoints body = do
    unwrapped <- JSON.withArray "Array Wrapper" unwrapArray body
    JSON.withObject "Datapoints" (`JSONT.parseField` "datapoints") unwrapped

