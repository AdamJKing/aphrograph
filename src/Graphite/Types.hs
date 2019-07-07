{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
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
import           Text.Show                     as TS
import           Network.HTTP.Req

newtype Time = Time { timestamp :: POSIXTime }
    deriving ( Show, Eq, Ord, Num, Real, Enum, Fractional, RealFrac, Scalable )

data GraphiteRequest = RenderRequest {
  from :: From, to :: Maybe To, target :: Target
}

data GraphiteUrl = forall s. GraphiteUrl (Url s)

instance Show GraphiteUrl where
    show (GraphiteUrl url) = TS.show url

data GraphiteError = HttpError HttpException | ParsingError Text deriving Show

instance Exception GraphiteError

type Target = Text

instance FormatTime Time where
    formatCharacter char = do
        fmtF <- formatCharacter char
        return $ \locale numericPad mwidth ->
            fmtF locale numericPad mwidth . toUTC

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
deltaTime' step earliest latest =
    if earliest == latest then 0 else floor $ (latest - earliest) / step

newtype Value = Value Decimal
    deriving newtype ( Show, Eq, Ord, Num, Fractional, Real, RealFrac )
    deriving ( Generic, Scalable )

data DataPoint = DataPoint { value :: Value, time :: Time }
    deriving ( Show, Eq )

newtype From = From Text
    deriving ( Show, Eq, IsString , ToHttpApiData)

newtype To = To Text
    deriving ( Show, Eq, IsString , ToHttpApiData)

instance JSON.FromJSON Time where
    parseJSON = fmap Time . JSON.parseJSON @NominalDiffTime

instance JSON.FromJSON Value where
    parseJSON (JSON.Number n) = return $ Value (realFracToDecimal 8 n)
    parseJSON _               = fail "value"

instance JSON.FromJSON DataPoint where
    parseJSON (JSON.Array arr) = case toList arr of
        [JSON.Null, t] -> DataPoint (Value 0.0) <$> JSON.parseJSON t
        [v        , t] -> DataPoint <$> JSON.parseJSON v <*> JSON.parseJSON t
        _              -> fail "Couldn't parse datapoint"
    parseJSON invalid = JSON.typeMismatch "DataPoint" invalid

newtype GraphiteResponse a = GraphiteResponse a

class MonadGraphite m where
  getMetrics :: GraphiteRequest -> m [DataPoint]
