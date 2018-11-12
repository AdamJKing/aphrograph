{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphite
  ( getMetricsForPast
  , getValuesInTimeRange
  , MetricResponse(..)
  , DataPoint(..)
  )
where

import           Control.Monad.IO.Class
import           Data.Text.Prettyprint.Doc
import           Control.Monad.Log
import           Control.Lens
import           Data.Aeson                    as JSON
import           Data.ByteString.Lazy           ( ByteString )
import qualified Data.Text                     as T
import qualified Data.Vector                   as V
                                                ( toList )
import           GHC.Generics
import           Network.Wreq
import           Network.Wreq.Lens
import           Time.System                    ( timeCurrent )
import           Time.Types                     ( Elapsed(..)
                                                , Seconds(..)
                                                , TimeInterval
                                                , toSeconds
                                                )


data DataPoint = DataPoint
  { value :: Double
  , time  :: Elapsed
  } deriving (Show)

instance Eq DataPoint where
  a == b = time a == time b

instance Ord DataPoint where
  compare a b = compare (time a) (time b)

data MetricResponse = MetricResponse
  { target     :: String
  , datapoints :: [DataPoint]
  } deriving (Show, Generic, JSON.FromJSON)

deriving instance Generic Seconds

deriving instance JSON.FromJSON Seconds

deriving instance Generic Elapsed

deriving instance JSON.FromJSON Elapsed

instance JSON.FromJSON DataPoint where
  parseJSON (JSON.Array arr) =
    case V.toList arr of
      [JSON.Null, t] -> DataPoint 0.0 <$> parseJSON t
      [v, t]    -> DataPoint <$> parseJSON v <*> parseJSON t
      _ -> fail "Couldn't parse datapoint"
  parseJSON _ = fail "Couldn't parse datapoint"

defaultArgs :: Network.Wreq.Lens.Options
defaultArgs = set params [format, timespan] defaults
 where
  format   = (T.pack "format", T.pack "json")
  timespan = (T.pack "from", T.pack "-100hr")

getValuesInTimeRange :: (Elapsed, Elapsed) -> [DataPoint] -> [DataPoint]
getValuesInTimeRange (a, b) = if a == b
  then error "Cannot find values without range"
  else filter (isInRange . time)
  where isInRange s = a <= s && s <= b

parseMetricTimeSeries :: ByteString -> [DataPoint]
parseMetricTimeSeries rawJson =
  case (eitherDecode rawJson :: Either String [MetricResponse]) of
    Right (mr : _) -> datapoints mr
    _              -> []

getMetricsForPast
  :: (MonadLog (Doc String) m, Show t, TimeInterval t, MonadIO m)
  => T.Text
  -> t
  -> m [DataPoint]
getMetricsForPast target timeSpan = do
  logMessage $ pretty "Making a call to graphite."
  let args = over
        params
        (++ [(T.pack "target", target), (T.pack "from", convert timeSpan)])
        defaultArgs
  response      <- liftIO $ getWith args "http://localhost/render"
  (Elapsed now) <- liftIO timeCurrent
  let datapoints = parseMetricTimeSeries (view responseBody response)
  let timespan   = (Elapsed (now - toSeconds timeSpan), Elapsed now)
  logMessage . pretty $ "Size of returned data was: " ++ show
    (length datapoints)
  return $ getValuesInTimeRange timespan datapoints
  where convert s = T.pack $ "-" ++ show s
