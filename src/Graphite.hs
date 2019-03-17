{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Graphite
  ( getValuesInTimeRange
  , MonadGraphite(..)
  , DataPoint(..)
  , Time(..)
  , timeAsSeconds
  , Value(..)
  )
where

import           Fmt
import           Control.Monad.Log
import           App
import           Control.Lens            hiding ( from
                                                , to
                                                )
import qualified Data.Aeson                    as JSON
import           Data.Aeson                     ( (.:) )
import qualified Data.Aeson.Types              as JSON
import           Network.Wreq
import           Graphite.Types


getValuesInTimeRange :: (Time, Time) -> [DataPoint] -> [DataPoint]
getValuesInTimeRange (a, b) = if a == b
  then error "Cannot find values without range"
  else filter (isInRange . time)
  where isInRange s = a <= s && s <= b

responseParser :: JSON.Value -> JSON.Parser [DataPoint]
responseParser str = do
  (x : _) <- JSON.parseJSONList str
  dps     <- x .: "datapoints"
  JSON.parseJSON dps

parseMetricTimeSeries :: LByteString -> [DataPoint]
parseMetricTimeSeries str = case decodeResp str of
  Right resp -> resp
  Left  _    -> []
 where
  decodeResp s = do
    v <- JSON.eitherDecode s
    JSON.parseEither responseParser v

class (Monad m) => MonadGraphite m where
  getMetricsForPast :: Text -> From -> To -> m [DataPoint]

instance (MonadIO m) => MonadGraphite (AppT m) where
  getMetricsForPast target from to =
    let parameters = constructQueryParams target from to
    in
      do
        logMessage "Making a call to graphite."
        response <- liftIO $ getWith parameters "http://localhost/render"
        let datapoints = parseMetricTimeSeries (view responseBody response)
        logMessage
          $  fmt "Size of returned data was: "
          +| length datapoints
          |+ "."
        logMessage $ fmt "Datapoints: " +|| datapoints ||+ ""
        return datapoints

instance (MonadGraphite m) => MonadGraphite (ReaderT a m) where
  getMetricsForPast a b c = lift $ getMetricsForPast a b c

constructQueryParams :: Text -> From -> To -> Options
constructQueryParams target (From from) (To to) =
  let targetArg = ("target", target)
      fromArg   = ("from", from)
      toArg     = ("to", to)
      jsonArg   = ("format", "json")
  in  set params [targetArg, fromArg, toArg, jsonArg] defaults
