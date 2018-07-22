{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Graphite
  ( getCurrentValue
  , metricRespToText
  , MetricResponse(..)
  , DataPoint(..)
  ) where

import           Control.Lens
import           Data.Aeson                 (FromJSON (..), Value (..))
import           Data.Aeson                 (Array, Value (Number),
                                             eitherDecode)
import           Data.ByteString.Lazy.Char8 (unpack)
import           Data.Maybe                 (fromMaybe)
import qualified Data.Vector                as V (toList)
import           GHC.Generics
import           Network.Wreq
import           Network.Wreq.Lens

data DataPoint = DataPoint
  { value :: Maybe Float
  , time  :: Integer
  }

data MetricResponse = MetricResponse
  { target     :: String
  , datapoints :: [DataPoint]
  } deriving (Generic, FromJSON)

instance FromJSON DataPoint where
  parseJSON (Array arr) =
    case (V.toList arr) of
      [v, t] -> DataPoint <$> value' <*> time'
        where value' = parseJSON v
              time' = parseJSON t

defaultArgs = defaults & params .~ [format, target, from]
  where
    format = ("format", "json")
    target = ("target", "stats.statsd.graphiteStats.flush_time")
    from = ("from", "-10min")

metricRespToText :: MetricResponse -> String
metricRespToText (MetricResponse {..}) = fromMaybe "No value" $ show <$> getLast datapoints
  where
    getLast ds = value . last $ ds

getCurrentValue :: IO String
getCurrentValue = do
  body <- getWith defaultArgs "http://localhost:80/render"
  let jsn = body ^. responseBody
  case (eitherDecode jsn :: Either String [MetricResponse]) of
    Right (mr:_) -> return $ metricRespToText mr
    Left err -> return $ "couldn't parse metric response: " ++ err ++ "\n JSON: " ++ (unpack jsn)
