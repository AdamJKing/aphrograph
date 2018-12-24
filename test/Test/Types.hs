{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module Test.Types where

import           Test.QuickCheck
import           Data.Hourglass          hiding ( Time )
import           Graphite
import           System.Random

newtype TestValue = TestValue (Value)
    deriving (Arbitrary, Random) via Double
    deriving (Eq, Ord, Show)

newtype TestTime = TestTime Time
    deriving (Arbitrary, Random) via Int64
    deriving (Eq, Ord, Show)
