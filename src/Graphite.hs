{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module Graphite
  ( getMetricsForPast
  , getValuesInTimeRange
  , MetricResponse(..)
  , DataPoint(..)
  )
where

import           Control.Lens
import           Data.Aeson                     ( FromJSON(..)
                                                , Value(..)
                                                , Array
                                                , eitherDecode
                                                , withScientific
                                                )
import           Data.ByteString.Lazy.Char8     ( unpack )
import           Data.ByteString.Lazy           ( ByteString(..) )
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Vector                   as V
                                                ( toList )
import           GHC.Generics
import           Network.Wreq
import           Network.Wreq.Lens
import           Time.Types                     ( Elapsed(..)
                                                , Seconds(..)
                                                , Minutes(..)
                                                , TimeInterval
                                                , toSeconds
                                                , fromSeconds
                                                )
import           Data.Hourglass                 ( timeDiff )
import           Time.System                    ( timeCurrent )
import           Data.Scientific                ( toBoundedInteger )

data DataPoint = DataPoint
  { value :: Float
  , time  :: Elapsed
  } deriving (Show, Eq)

instance Ord DataPoint where
  compare a b = compare (time a) (time b)

data MetricResponse = MetricResponse
  { target     :: String
  , datapoints :: [DataPoint]
  } deriving (Show, Generic, FromJSON)

deriving instance Generic Seconds
deriving instance FromJSON Seconds

deriving instance Generic Elapsed
deriving instance FromJSON Elapsed

instance FromJSON DataPoint where
  parseJSON (Array arr) = case V.toList arr of
      [Null, t] -> DataPoint 0.0 <$> parseJSON t
      [v, t] -> DataPoint <$> parseJSON v <*> parseJSON t

defaultArgs = defaults & params .~ [format, target, from]
 where
  format = ("format", "json")
  target = ("target", "randomWalk(\"test\")")
  from   = ("from", "-100hr")

getValuesInTimeRange :: (Elapsed, Elapsed) -> [DataPoint] -> [DataPoint]
getValuesInTimeRange (a, b) = filter (isInRange . time)
  where isInRange s = a <= s && s <= b

parseMetricTimeSeries :: ByteString -> [DataPoint]
parseMetricTimeSeries rawJson =
  case (eitherDecode rawJson :: Either String [MetricResponse]) of
    Right (mr : _) -> datapoints mr
    Left  err      -> []

getMetricsForPast :: Seconds -> IO [DataPoint]
getMetricsForPast timeSpan = do
  resp          <- getWith defaultArgs "http://localhost/render"
  (Elapsed now) <- timeCurrent
  let datapoints = parseMetricTimeSeries (resp ^. responseBody)
  return
    $ getValuesInTimeRange (Elapsed (now - timeSpan), Elapsed now) datapoints
