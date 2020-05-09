{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ArbitraryInstances where

import App.Components
import qualified App.State as App
import Brick.Widgets.List as BWL
import DerivedArbitraryInstances
import Display.Graph as Graph
import Display.Labels
import Graphics.Vty.Input.Events as Vty
import Graphite.Types as Graphite
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Req as Req
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.ADT
import Test.QuickCheck.Instances.Time ()
import Test.QuickCheck.Instances.Vector
  (
  )

instance Arbitrary App.ActiveState where
  arbitrary = do
    _metricsView <- Just . (BWL.list MetricsBrowserComponent ?? 1) <$> arbitrary
    _graphData <- arbitrary
    _timezone <- arbitrary
    return (App.ActiveState {..})

deriving via (GenArbitrary GraphiteError) instance Arbitrary GraphiteError

deriving via (GenArbitrary App.GraphData) instance Arbitrary App.GraphData

deriving via (GenArbitrary App.Error) instance Arbitrary App.Error

deriving via (GenArbitrary App.FailedState) instance Arbitrary App.FailedState

deriving via (GenArbitrary App.CurrentState) instance Arbitrary App.CurrentState

instance Arbitrary DataPoint where
  arbitrary = genericArbitrary

instance Arbitrary GraphiteRequest where
  arbitrary = genericArbitrary

instance (Ord x, Arbitrary x, Ord y, Arbitrary y) => Arbitrary (Graph x y) where
  arbitrary = Graph.mkGraph <$> arbitrary

data Range i = Range {lower :: i, higher :: i}
  deriving (Show, Eq)

deriving instance ToADTArbitrary TimeStep

instance Arbitrary TimeStep where
  arbitrary = genericArbitrary

instance (Arbitrary i, Num i, Ord i) => Arbitrary (Range i) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary `suchThat` (/= a)
    return $ Range (min a b) (max a b)

instance Arbitrary Vty.Modifier where
  arbitrary = genericArbitrary

deriving instance ToADTArbitrary Vty.Modifier

instance Arbitrary Vty.Key where
  arbitrary = genericArbitrary

deriving instance ToADTArbitrary Vty.Key

instance Arbitrary Vty.Button where
  arbitrary = genericArbitrary

instance Arbitrary Time where
  arbitrary = fromInteger <$> arbitrary

instance Random Time where
  randomR range = first fromInteger . randomR (toInts range) where toInts = join bimap (round . timestamp)

  random = randomR (0, 1)

instance Arbitrary Http.HttpException where
  arbitrary = return $ Http.HttpExceptionRequest "http://www.example.com" Http.ResponseTimeout

deriving via (GenArbitrary Req.HttpException) instance Arbitrary Req.HttpException
