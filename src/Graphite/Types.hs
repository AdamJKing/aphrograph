{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Graphite.Types where

import qualified Data.Aeson                    as JSON
import qualified Data.Aeson.Types              as JSON
import           Data.Decimal
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Data.Time.Format
import           Data.Time.LocalTime
import           Web.HttpApiData
import           Display.Projection.Scalable
import           Network.HTTP.Req
import           Network.HTTP.Client           as HTTP
import           Data.Typeable
import qualified Text.Show                     as TS
import           Control.Lens.TH                ( makePrisms )

newtype Time = Time { timestamp :: POSIXTime }
    deriving newtype ( Show, Eq, Ord, Num, Real, Enum, Fractional, RealFrac, Scalable )

newtype Value = Value Decimal
    deriving newtype ( Show, Eq, Ord, Num, Fractional, Real, RealFrac, Scalable )
    deriving Generic

data DataPoint = DataPoint { value :: Value, time :: Time }
    deriving ( Show, Eq )

newtype From = From Text
    deriving newtype ( Show, Eq, IsString , ToHttpApiData)

newtype To = To Text
    deriving newtype ( Show, Eq, IsString , ToHttpApiData)

type Target = Text

data GraphiteRequest = RenderRequest {
  _from :: From,
  _to :: Maybe To,
  _target :: Target
}
  deriving (Eq, Show, Generic)

data GraphiteUrl where
    GraphiteUrl ::Typeable s => Url ( s :: Scheme ) -> GraphiteUrl

instance Eq GraphiteUrl where
  (GraphiteUrl url) == (GraphiteUrl url') = Just url == cast url'

instance Show GraphiteUrl where
  show (GraphiteUrl url) = show url

data GraphiteError = HttpError HTTP.HttpException | ParsingError Text
  deriving ( Show, Generic )
  deriving anyclass Exception

makePrisms ''GraphiteError

instance FormatTime Time where
  formatCharacter char = do
    fmtF <- formatCharacter char
    return $ \locale numericPad mwidth -> fmtF locale numericPad mwidth . toUTC

millisecond :: Time
millisecond = 0.001

toLocalTime :: TimeZone -> Time -> LocalTime
toLocalTime timezone = utcToLocalTime timezone . toUTC

toUTC :: Time -> UTCTime
toUTC = posixSecondsToUTCTime . timestamp

toTimeOfDay :: Time -> TimeOfDay
toTimeOfDay = timeToTimeOfDay . utctDayTime . toUTC

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
  parseJSON _               = Prelude.fail "value"

instance JSON.FromJSON DataPoint where
  parseJSON (JSON.Array arr) = case toList arr of
    [JSON.Null, t] -> DataPoint (Value 0.0) <$> JSON.parseJSON t
    [v        , t] -> DataPoint <$> JSON.parseJSON v <*> JSON.parseJSON t
    _              -> Prelude.fail "Couldn't parse datapoint"
  parseJSON invalid = JSON.typeMismatch "DataPoint" invalid

newtype Metric = Metric Text
  deriving newtype (Show, Eq, JSON.FromJSON, IsString)
  deriving Generic

class Monad m => MonadGraphite m where
  listMetrics :: m [Metric]
  getMetrics :: GraphiteRequest -> m [DataPoint]

data MetricsResponse = MetricsResponse {
    target :: Text,
    tags :: Map Text Text,
    datapoints :: [DataPoint]
} deriving ( Show, Eq, Generic )

instance JSON.FromJSON MetricsResponse
