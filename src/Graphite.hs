{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Graphite
  ( getValuesInTimeRange
  , DataPoint(..)
  , GraphiteM(runGraphite)
  , MonadGraphite(..)
  , Time(..)
  , timeAsSeconds
  , Value(..)
  )
where

import           Fmt
import           Control.Monad.Log
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

newtype Time = Time Elapsed deriving (Show, Eq, Ord, Num, Timeable)

timeAsSeconds :: Time -> Seconds
timeAsSeconds (Time (Elapsed s)) = s

newtype Value = Value Decimal deriving (Show, Eq, Ord, Generic, Num, Fractional, Real, RealFrac)

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

parseMetricTimeSeries :: (MonadLog Text m) => LByteString -> m [DataPoint]
parseMetricTimeSeries str = case decodeResp str of

  Right resp -> return resp

  Left  err  -> do
    logMessage (toText err)
    return []

 where
  decodeResp s = do
    v <- JSON.eitherDecode s
    JSON.parseEither responseParser v

class MonadGraphite m where
  getMetricsForPast :: (Show t, TimeInterval t) => Text -> t -> m [DataPoint]

newtype GraphiteM a = GraphiteM {
  runGraphite :: LoggingT Text IO a
} deriving ( Functor, Applicative, Monad , MonadIO)

instance MonadGraphite GraphiteM where
  getMetricsForPast target timeSpan = GraphiteM $ do
    logMessage "Making a call to graphite."
    let args = over
          params
          (++ [("target", target), ("from", '-' `cons` show timeSpan)])
          defaultArgs
    response      <- liftIO $ getWith args "http://localhost/render"
    (Elapsed now) <- liftIO timeCurrent
    datapoints    <- parseMetricTimeSeries (view responseBody response)
    let timespan = (fromIntegral $ now - toSeconds timeSpan, fromIntegral now)
    logMessage $ fmt "Size of returned data was: " +| length datapoints |+ "."
    return $ getValuesInTimeRange timespan datapoints
