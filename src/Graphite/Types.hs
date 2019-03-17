{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}

module Graphite.Types where

import qualified Data.Aeson                    as JSON
import           Data.Hourglass                 ( Elapsed(..)
                                                , Seconds(..)
                                                , Timeable
                                                )
import           Display.Projection.Scalable
import           Data.Decimal
import           Data.Scientific


newtype Time = Time Elapsed
  deriving newtype (Show, Eq, Ord, Num, Timeable)
  deriving (Enum, Real, Integral) via Seconds
  deriving Scalable via Int64

timeAsSeconds :: Time -> Seconds
timeAsSeconds (Time (Elapsed s)) = s

newtype Value = Value Decimal
  deriving newtype (Show, Eq, Ord, Num, Fractional, Real, RealFrac)
  deriving (Generic , Scalable)

instance JSON.FromJSON Value where
  parseJSON (JSON.Number n) = return $ Value (realFracToDecimal 8 n)
  parseJSON _               = fail "value"

newtype From = From Text deriving (Show, Eq, IsString)

newtype To = To Text deriving (Show, Eq, IsString)

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
