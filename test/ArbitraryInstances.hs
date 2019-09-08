{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module ArbitraryInstances where

import           App.Args                      as App
                                                ( Args(..) )
import           DerivedArbitraryInstances
import           Display.Graph                 as Graph
import           Display.Types

import           Graphics.Vty.Input.Events     as Vty

import           Graphite.Types                as Graphite

import           System.Random

import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.ADT
import           Test.QuickCheck.Instances.Time ( )
import           Network.HTTP.Req               ( http
                                                , https
                                                )
import           Display.Labels
import           App
import qualified Network.HTTP.Req              as Req
import qualified Network.HTTP.Client           as Http

instance Arbitrary ActiveState where
    arbitrary = do
        _metricsView <- arbitrary
        _graphData   <- arbitrary
        _timezone    <- arbitrary
        _appArgs     <- arbitrary
        let _logger = const pass
        return (ActiveState { .. })

deriving via (GenArbitrary GraphiteError) instance Arbitrary GraphiteError
deriving via (GenArbitrary AppError) instance Arbitrary AppError

deriving via (GenArbitrary AppState) instance Arbitrary AppState

instance Arbitrary DataPoint where
    arbitrary = genericArbitrary

instance Arbitrary GraphiteRequest where
    arbitrary = genericArbitrary

deriving instance (Generic n) => Generic (Dimensions n)

instance (Generic n, Arbitrary n) => Arbitrary (Dimensions n) where
    arbitrary = genericArbitrary

instance (Ord x, Arbitrary x, Ord y, Arbitrary y) => Arbitrary (Graph x y) where
    arbitrary = Graph.mkGraph <$> arbitrary

data Range i = Range { lower :: i, higher :: i }
    deriving ( Show, Eq )

deriving instance ToADTArbitrary TimeStep

instance Arbitrary a => Arbitrary ( App a ) where
    arbitrary = return (liftIO (generate arbitrary))

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
    randomR range = first fromInteger . randomR (toInts range)
        where toInts = join bimap (round . timestamp)

    random = randomR (0, 1)

instance Arbitrary App.Args where
    arbitrary = do
        fromTime    <- arbitrary
        toTime      <- arbitrary
        targetArg   <- arbitrary
        graphiteUrl <- oneof
            [ return (GraphiteUrl (http "example.com"))
            , return (GraphiteUrl (https "example.com"))
            ]
        let debugMode = False
        return (App.Args fromTime toTime targetArg graphiteUrl debugMode)

instance Arbitrary Http.HttpException where
    arbitrary = frequency
        [ (10, applyArbitrary2 Http.InvalidUrlException)
        , ( 90
          , return $ Http.HttpExceptionRequest "http://www.example.com"
                                               Http.ResponseTimeout
          )
        ]

deriving via (GenArbitrary Req.HttpException) instance Arbitrary Req.HttpException
