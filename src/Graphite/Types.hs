{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Graphite.Types where

import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import Data.ByteString as BS (length)
import Data.Decimal (Decimal, realFracToDecimal)
import Data.Text.Encoding.Error (ignore)
import Data.Time.Clock (NominalDiffTime, UTCTime)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import Data.Time.Format (FormatTime)
import Data.Time.LocalTime (LocalTime, TimeZone, utcToLocalTime)
import Data.Typeable (cast)
import Data.Vector (Vector)
import Display.Projection.Scalable (Scalable)
import Network.HTTP.Client as HTTP (HttpException)
import Network.HTTP.Req (Scheme, Url)
import qualified Text.Show as TS
import Web.HttpApiData (ToHttpApiData)

newtype Time = Time {timestamp :: POSIXTime}
  deriving newtype (Show, Eq, Ord, Num, Real, Enum, Fractional, RealFrac, Scalable, FormatTime)

newtype Value = Value Decimal
  deriving newtype (Show, Eq, Ord, Num, Fractional, Real, RealFrac, Scalable)
  deriving (Generic)

data DataPoint = DataPoint {value :: Value, time :: Time}
  deriving (Show, Eq)

newtype From = From Text
  deriving newtype (Show, Eq, IsString, ToHttpApiData)

newtype To = To Text
  deriving newtype (Show, Eq, IsString, ToHttpApiData)

data GraphiteRequest = RenderRequest
  { requestFrom :: From,
    requestTo :: Maybe To,
    requestMetric :: Metric,
    preferredTimeZone :: TimeZone
  }
  deriving (Eq, Show, Generic)

data GraphiteUrl where
  GraphiteUrl :: Typeable s => Url (s :: Scheme) -> GraphiteUrl

instance Eq GraphiteUrl where
  (GraphiteUrl url) == (GraphiteUrl url') = Just url == cast url'

instance Show GraphiteUrl where
  show (GraphiteUrl url) = show url

data GraphiteError = HttpError HTTP.HttpException | ParsingError Text
  deriving (Show, Generic)
  deriving anyclass (Exception)

toLocalTime :: TimeZone -> Time -> LocalTime
toLocalTime timezone = utcToLocalTime timezone . toUTC

toUTC :: Time -> UTCTime
toUTC = posixSecondsToUTCTime . timestamp

deltaDays :: Time -> Time -> Int
deltaDays = deltaTime' 86400

deltaHours :: Time -> Time -> Int
deltaHours = deltaTime' 3600

deltaMinutes :: Time -> Time -> Int
deltaMinutes = deltaTime' 60

deltaSeconds :: Time -> Time -> Int
deltaSeconds = deltaTime' 1

deltaTime' :: Time -> Time -> Time -> Int
deltaTime' step earliest latest = if earliest == latest then 0 else floor $ (latest - earliest) / step

instance JSON.FromJSON Time where
  parseJSON = fmap Time . JSON.parseJSON @NominalDiffTime

instance JSON.FromJSON Value where
  parseJSON (JSON.Number n) = return $ Value (realFracToDecimal 8 n)
  parseJSON _ = fail "value"

instance JSON.FromJSON DataPoint where
  parseJSON (JSON.Array arr) = case toList arr of
    [JSON.Null, t] -> DataPoint (Value 0.0) <$> JSON.parseJSON t
    [v, t] -> DataPoint <$> JSON.parseJSON v <*> JSON.parseJSON t
    _unexpected -> fail "Couldn't parse datapoint"
  parseJSON invalid = JSON.typeMismatch "DataPoint" invalid

newtype Metric = Metric ByteString
  deriving newtype (IsString)
  deriving (Show, Eq, Generic)

instance ToText Metric where
  toText (Metric descriptor) = decodeUtf8With ignore descriptor

instance JSON.FromJSON Metric where
  parseJSON value =
    Metric <$> JSON.withText "Metric" (pure . encodeUtf8) value

metricLength :: Metric -> Int
metricLength (Metric txt) = BS.length txt

class Monad m => MonadGraphite m where
  listMetrics :: m (Vector Metric)

  getMetrics :: GraphiteRequest -> m [DataPoint]

data MetricsResponse = MetricsResponse
  { target :: Text,
    tags :: Map Text Text,
    datapoints :: [DataPoint]
  }
  deriving (Show, Eq, Generic)

instance JSON.FromJSON MetricsResponse
