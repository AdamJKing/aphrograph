{-# LANGUAGE DataKinds #-}
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
import Data.Decimal
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Format
import Data.Time.LocalTime
import Data.Typeable
import Display.Projection.Scalable
import Network.HTTP.Client as HTTP
import Network.HTTP.Req
import Relude
import qualified Text.Show as TS
import Web.HttpApiData

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

type Target = Text

data GraphiteRequest = RenderRequest
  { _from :: From,
    _to :: Maybe To,
    _target :: Target
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
    _unexpected -> Relude.fail "Couldn't parse datapoint"
  parseJSON invalid = JSON.typeMismatch "DataPoint" invalid

newtype Metric = Metric Text
  deriving newtype (Show, Eq, JSON.FromJSON, IsString)
  deriving (Generic)

class Monad m => MonadGraphite m where
  listMetrics :: m [Metric]
  getMetrics :: GraphiteRequest -> m [DataPoint]

data MetricsResponse = MetricsResponse
  { target :: Text,
    tags :: Map Text Text,
    datapoints :: [DataPoint]
  }
  deriving (Show, Eq, Generic)

instance JSON.FromJSON MetricsResponse
