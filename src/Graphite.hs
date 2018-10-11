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

import           Data.String
import           Data.Text.Prettyprint.Doc
import           Control.Monad.Log
import           Streaming
import qualified Streaming.Prelude             as S
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
import           Data.Scientific                
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
  { value :: Scientific
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
  format = (T.pack "format", T.pack "json")
  from   = (T.pack "from", T.pack "-100hr")

getValuesInTimeRange :: (Elapsed, Elapsed) -> [DataPoint] -> [DataPoint]
getValuesInTimeRange (a, b) = filter (isInRange . time)
  where isInRange s = a <= s && s <= b

parseMetricTimeSeries :: ByteString -> [DataPoint]
parseMetricTimeSeries rawJson =
  case (eitherDecode rawJson :: Either String [MetricResponse]) of
    Right (mr : _) -> datapoints mr
    Left  err      -> []

getMetricsForPast
  :: (MonadLog (Doc String) m, TimeInterval t, MonadIO m)
  => T.Text
  -> t
  -> m [DataPoint]
getMetricsForPast target timeSpan = do
  logMessage $ pretty "Making a call to graphite."
  let args = over params (++ [(T.pack "target", target)]) defaultArgs
  response      <- liftIO $ getWith args "http://localhost/render"
  (Elapsed now) <- liftIO timeCurrent
  let datapoints = parseMetricTimeSeries (view responseBody response)
  let timespan   = (Elapsed (now - toSeconds timeSpan), Elapsed now)
  logMessage . pretty $ "Size of returned data was: " ++ show
    (length datapoints)
  return $ getValuesInTimeRange timespan datapoints
