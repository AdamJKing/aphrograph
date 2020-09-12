{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module CommonProperties
  ( appProp,
    daysFrom,
    range,
    getMetricsResponse,
    listMetricsResponse,
    assertAll,
    TestM,
    EventOutcome (..),
  )
where

import App.Components (GraphViewer (..))
import qualified App.Config as App
import qualified App.State as App
import ArbitraryInstances ()
import Control.Lens.TH (makeLenses)
import Control.Monad.Base (MonadBase (..))
import Control.Monad.Except (MonadError, liftEither)
import Control.Monad.Trans.Control (MonadBaseControl (..))
import DerivedArbitraryInstances (GenArbitrary (..))
import Events.Types (MonadOutcome (..))
import Graphite.Types
  ( DataPoint,
    Metric,
    MonadGraphite (..),
    Time,
  )
import Test.Hspec (Spec)
import Test.Hspec.QuickCheck (prop)
import Test.Orphans ()
import Test.QuickCheck (Discard (Discard), Property)
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary))
import Test.QuickCheck.Gen (Gen (..))
import Test.QuickCheck.GenT (MonadGen (liftGen), suchThat)
import Test.QuickCheck.Monadic (PropertyM, assert, monadic)
import Test.QuickCheck.Property (Testable (..), counterexample)

range :: (Ord a, Arbitrary a) => Gen (a, a)
range = do
  lower <- liftGen arbitrary
  upper <- liftGen arbitrary `suchThat` (> lower)
  return (lower, upper)

daysFrom :: Word16 -> Time -> [Time]
daysFrom n = take (fromIntegral n + 1) . iterate (+ 86400)

deriving instance (Show r, Arbitrary r, Testable (m a)) => Testable (ReaderT r m a)

appProp :: (HasCallStack, Testable a) => String -> PropertyM Gen a -> Spec
appProp desc = prop desc . monadic property

newtype TestM a = TestM {_runTest :: ExceptT App.Error (ReaderT App.Config Gen) a}
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadReader App.Config,
      MonadError App.Error,
      MonadGen
    )

runTestM :: App.Config -> TestM a -> Gen (Either App.Error a)
runTestM conf = usingReaderT conf . runExceptT . _runTest

eitherProp :: (Testable prop, Show a) => Either a prop -> Property
eitherProp (Right p) = property p
eitherProp (Left err) = counterexample (show err) $ property Discard

instance Testable prop => Testable (TestM prop) where
  property test = property $ do
    conf <- arbitrary
    result <- runTestM conf test
    return (eitherProp result)

instance MonadBase Gen TestM where
  liftBase = liftGen

instance MonadBaseControl Gen TestM where
  type StM TestM a = Either App.Error a

  liftBaseWith action = do
    conf <- ask
    liftGen (action (runTestM conf))

  restoreM = liftEither

data EventOutcome = Continue | Stop
  deriving (Generic, Eq, Show)
  deriving (Arbitrary) via (GenArbitrary EventOutcome)

instance MonadOutcome TestM where
  type EventF TestM = (,) EventOutcome
  continue = return . (Continue,)
  stop = return . (Stop,)

data MockGraphiteResponses = MockResponses
  { _listMetricsResponse :: Either App.Error [Metric],
    _getMetricsResponse :: Either App.Error [DataPoint]
  }

makeLenses ''MockGraphiteResponses

instance MonadGraphite TestM where
  listMetrics = liftGen arbitrary
  getMetrics _ = liftGen arbitrary

instance GraphViewer TestM where
  triggerUpdate _ = pure ()
  updateGraph _ = liftGen arbitrary

assertAll :: (Foldable t, Monad m) => (a -> Bool) -> t a -> PropertyM m ()
assertAll predicate = assert . all predicate
