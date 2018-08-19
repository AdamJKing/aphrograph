{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}

module Graphite
  ( getMetricsForPast
  , getValuesInTimeRange
  , MetricResponse(..)
  , DataPoint(..)
  )
where

import           Control.Lens
import           Data.Aeson                     ( Array
                                                , FromJSON(..)
                                                , Value(..)
                                                , eitherDecode
                                                , withScientific
                                                )
import           Data.ByteString.Lazy           ( ByteString(..) )
import           Data.ByteString.Lazy.Char8     ( unpack )
import           Data.Hourglass                 ( timeDiff )
import           Data.Maybe                     ( fromMaybe )
import           Data.Scientific                ( toBoundedInteger )
import qualified Data.Text                     as T
import qualified Data.Vector                   as V
                                                ( toList )
import           GHC.Generics
import           Network.Wreq
import           Network.Wreq.Lens
import           Time.System                    ( timeCurrent )
import           Time.Types                     ( Elapsed(..)
                                                , Minutes(..)
                                                , Seconds(..)
                                                , TimeInterval
                                                , fromSeconds
                                                , toSeconds
                                                )

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
  parseJSON (Array arr) =
    case V.toList arr of
      [Null, t] -> DataPoint 0.0 <$> parseJSON t
      [v, t]    -> DataPoint <$> parseJSON v <*> parseJSON t

defaultArgs = set params [format, from] defaults
 where
  format = ("format", "json")
  from   = ("from", "-100hr")

getValuesInTimeRange :: (Elapsed, Elapsed) -> [DataPoint] -> [DataPoint]
getValuesInTimeRange (a, b) = filter (isInRange . time)
  where isInRange s = a <= s && s <= b

parseMetricTimeSeries :: ByteString -> [DataPoint]
parseMetricTimeSeries rawJson =
  case (eitherDecode rawJson :: Either String [MetricResponse]) of
    Right (mr : _) -> datapoints mr
    Left  err      -> []

getMetricsForPast :: TimeInterval t => T.Text -> t -> IO [DataPoint]
getMetricsForPast target timeSpan = do
  let args = over params (++ [("target", target)]) defaultArgs
  response      <- getWith args "http://localhost/render"
  (Elapsed now) <- timeCurrent
  let datapoints = parseMetricTimeSeries (view responseBody response)
  let timespan   = (Elapsed (now - toSeconds timeSpan), Elapsed now)
  return $ getValuesInTimeRange timespan datapoints
