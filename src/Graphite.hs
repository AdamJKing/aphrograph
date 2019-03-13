{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Graphite
  ( getValuesInTimeRange
  , runGraphiteT
  , MonadGraphite(..)
  , DataPoint(..)
  , Time(..)
  , timeAsSeconds
  , Value(..)
  )
where

import           Fmt
import           Control.Monad.Log
import           Display.Projection.Scalable
import           Control.Lens
import qualified Data.Aeson                    as JSON
import           Data.Aeson                     ( (.:) )
import qualified Data.Aeson.Types              as JSON
import           Data.Scientific
import           Network.Wreq
import           Network.Wreq.Lens
import           Time.System                    ( timeCurrent )
import           Time.Types                     ( Elapsed(..)
                                                , Seconds(..)
                                                , TimeInterval
                                                , toSeconds
                                                )
import           Data.Hourglass          hiding ( Time )
import           Data.Decimal

newtype Time = Time Elapsed
  deriving newtype (Show, Eq, Ord, Num, Timeable)
  deriving (Enum, Real, Integral) via Seconds
  deriving Scalable via Int64

timeAsSeconds :: Time -> Seconds
timeAsSeconds (Time (Elapsed s)) = s

newtype Value = Value Decimal
  deriving newtype (Show, Eq, Ord, Num, Fractional, Real, RealFrac)
  deriving (Generic , Scalable)

instance JSON.FromJSON Value where
  parseJSON (JSON.Number n) = return $ Value (realFracToDecimal 8 n)
  parseJSON _               = fail "value"

data DataPoint = DataPoint
  { value :: Value
  , time  :: Time
  } deriving Show

instance Eq DataPoint where
  a == b = time a == time b

instance Ord DataPoint where
  compare a b = compare (time a) (time b)

instance JSON.FromJSON Time where
  parseJSON (JSON.Number n) = case (toBoundedInteger n :: Maybe Int64) of
    Just elapsed -> return . Time . fromIntegral $ elapsed
    Nothing      -> fail "Couldn't parse time, datapoint"
  parseJSON _ = fail "could not parse time"

instance JSON.FromJSON DataPoint where
  parseJSON (JSON.Array arr) = case toList arr of
    [JSON.Null, t] -> DataPoint (Value 0.0) <$> JSON.parseJSON t
    [v        , t] -> DataPoint <$> JSON.parseJSON v <*> JSON.parseJSON t
    _              -> fail "Couldn't parse datapoint"
  parseJSON _ = fail "Couldn't parse datapoint"

defaultArgs :: Network.Wreq.Lens.Options
defaultArgs = set params [("format", "json")] defaults

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
  getMetricsForPast :: (Show t, TimeInterval t) => Text -> t -> m [DataPoint]

-- instance !MonadIO => MonadGraphite

newtype GraphiteT m a = GraphiteT (m a) deriving newtype (Functor, Applicative, Monad, MonadIO)

runGraphiteT :: GraphiteT m a -> m a
runGraphiteT (GraphiteT f) = f

instance (MonadIO m) => MonadGraphite (GraphiteT m) where
  getMetricsForPast target timeSpan = do
    let args = constructArgs target timeSpan
    response      <- liftIO $ getWith args "http://localhost/render"
    (Elapsed now) <- liftIO timeCurrent
    let datapoints = parseMetricTimeSeries (view responseBody response)
    let timespan   = (fromIntegral $ now - toSeconds timeSpan, fromIntegral now)
    return $ getValuesInTimeRange timespan datapoints

instance (MonadGraphite m) => MonadGraphite (LoggingT Text m) where
  getMetricsForPast target timeSpan = do
    logMessage "Making a call to graphite."
    datapoints <- lift $ getMetricsForPast target timeSpan
    logMessage $ fmt "Size of returned data was: " +| length datapoints |+ "."
    logMessage $ fmt "Datapoints: " +|| datapoints ||+ ""
    return datapoints

instance (MonadGraphite m) => MonadGraphite (ReaderT r m) where
  getMetricsForPast target timeSpan = lift (getMetricsForPast target timeSpan)


constructArgs :: Show a => Text -> a -> Options
constructArgs target timeSpan = over
  params
  (++ [("target", target), ("from", '-' `cons` show timeSpan)])
  defaultArgs

