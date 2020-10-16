{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ArbitraryInstances where

import qualified App.Config as App
import qualified App.State as App
import Data.Time (utc)
import DerivedArbitraryInstances (GenArbitrary (..))
import Display.Graph as Graph (Graph, mkGraph)
import Display.GraphWidget
  ( GraphCanvasWidget (..),
    HorizontalAxisWidget (..),
    VerticalAxisWidget (..),
  )
import Display.Labels (TimeStep (..))
import Graphics.Vty.Input.Events as Vty
  ( Button,
    Key (..),
    Modifier (..),
  )
import Graphite.Types as Graphite
  ( DataPoint,
    GraphiteError (..),
    GraphiteRequest,
    Time (timestamp),
  )
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Req as Req
import System.Random (Random (random, randomR))
import Test.QuickCheck (Arbitrary (arbitrary), suchThat)
import Test.QuickCheck.Arbitrary.ADT
  ( ToADTArbitrary,
    genericArbitrary,
  )
import Test.QuickCheck.Instances.Time ()
import Test.QuickCheck.Instances.Vector ()

deriving via (GenArbitrary GraphCanvasWidget) instance Arbitrary GraphCanvasWidget

deriving via (GenArbitrary VerticalAxisWidget) instance Arbitrary VerticalAxisWidget

deriving via (GenArbitrary HorizontalAxisWidget) instance Arbitrary HorizontalAxisWidget

deriving via (GenArbitrary GraphiteError) instance Arbitrary GraphiteError

deriving via (GenArbitrary App.Error) instance Arbitrary App.Error

deriving via (GenArbitrary App.FailedState) instance Arbitrary App.FailedState

instance Arbitrary DataPoint where
  arbitrary = genericArbitrary

instance Arbitrary GraphiteRequest where
  arbitrary = genericArbitrary

instance (Ord x, Arbitrary x, Ord y, Arbitrary y) => Arbitrary (Graph x y) where
  arbitrary = Graph.mkGraph <$> arbitrary @[(x, y)]

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

instance Arbitrary App.Config where
  arbitrary = do
    graphiteConf <- arbitrary
    return $
      App.Config
        { _graphiteConfig = graphiteConf,
          _timezone = utc
        }
