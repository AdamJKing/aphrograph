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
  ( forAllEnvs,
    appProp,
    daysFrom,
    range,
    runMonadicTest,
    getMetricsResponse,
    listMetricsResponse,
    assertAll,
    activeState,
  )
where

import qualified App.Config as App
import qualified App.State as App
import ArbitraryInstances ()
import Control.Lens.Extras
import Control.Lens.TH (makeLenses)
import Control.Monad.Base
import Control.Monad.Trans.Control
import DerivedArbitraryInstances
import Events.Types
import Graphite.Types
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.Orphans ()
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen (Gen (..))
import Test.QuickCheck.GenT
import Test.QuickCheck.Monadic hiding (stop)
import Test.QuickCheck.Property
  ( Property,
    Result (..),
    Testable (..),
    failed,
  )

range :: (Ord a, Arbitrary a) => Gen (a, a)
range = do
  lower <- liftGen arbitrary
  upper <- liftGen arbitrary `suchThat` (> lower)
  return (lower, upper)

daysFrom :: Word16 -> Time -> [Time]
daysFrom n = take (fromIntegral n + 1) . iterate (+ 86400)

deriving instance (Show r, Arbitrary r, Testable (m a)) => Testable (ReaderT r m a)

readerProp :: Monad m => r -> PropertyM (ReaderT r m) a -> PropertyM m a
readerProp r = hoistLiftedPropertyM (`runReaderT` r)

forAllEnvs :: (Arbitrary r, Show r, Monad m) => PropertyM (ReaderT r m) a -> PropertyM m a
forAllEnvs op = pick arbitrary >>= (readerProp ?? op)

hoistGen :: (forall x. n x -> m x) -> Gen (n a) -> Gen (m a)
hoistGen nat (MkGen f) = MkGen $ \gen -> nat . f gen

hoistPropertyM :: (forall x. n x -> m x) -> (forall x. m x -> n x) -> PropertyM n a -> PropertyM m a
hoistPropertyM nat natInv (MkPropertyM f) = MkPropertyM $ \g -> hoistGen nat $ f $ hoistGen natInv . g

hoistLiftedPropertyM ::
  (MonadTrans t, Monad m, Coercible n (t m)) => (forall x. t m x -> m x) -> PropertyM n a -> PropertyM m a
hoistLiftedPropertyM lower = hoistPropertyM lower lift . coerce

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

instance MonadBase Gen TestM where
  liftBase = liftGen

instance MonadBaseControl Gen TestM where
  type StM TestM a = Either App.Error a

  liftBaseWith action = do
    conf <- ask
    liftGen (action (runTestM conf))

  restoreM = liftEither

data EventOutcome = Continue | Stop
  deriving (Generic)
  deriving (Arbitrary) via (GenArbitrary EventOutcome)

instance MonadOutcome TestM where
  type EventF TestM = (,) EventOutcome
  continue = return . (Continue,)
  stop = return . (Stop,)

data MockGraphiteResponses
  = MockResponses
      { _listMetricsResponse :: Either App.Error [Metric],
        _getMetricsResponse :: Either App.Error [DataPoint]
      }

makeLenses ''MockGraphiteResponses

instance Arbitrary s => MonadEventHandler e s TestM where
  handleEvent _ _ = liftGen arbitrary2

instance MonadGraphite TestM where
  listMetrics = liftGen arbitrary
  getMetrics _ = liftGen arbitrary

instance GraphViewer TestM where
  updateGraph = liftGen arbitrary

runMonadicTest :: Testable a => PropertyM TestM a -> Property
runMonadicTest =
  monadic
    ( \test -> property $ do
        env <- arbitrary
        result <- unwrapTest env test
        return (either unexpectedError property result)
    )
  where
    unwrapTest env = usingReaderT env . runExceptT . _runTest
    unexpectedError err = property $ failed {reason = "Unexpected exception.", theException = Just (SomeException err)}

assertAll :: (Foldable t, Monad m) => (a -> Bool) -> t a -> PropertyM m ()
assertAll predicate = assert . all predicate

activeState :: Gen (App.CurrentState' [])
activeState = arbitrary `suchThat` is App.active
