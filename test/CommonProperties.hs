{-# LANGUAGE AllowAmbiguousTypes #-}
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
{-# OPTIONS_GHC -fno-warn-orphans #-}

module CommonProperties
  ( shouldThrowMatching,
    forAllEnvs,
    throwingErrors,
    ignoreLogging,
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
import Control.Monad.Log
import Events
import Events.Types
import Graphite.Types
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
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
  x <- liftGen arbitrary
  y <- liftGen arbitrary `suchThat` (/= x)
  return (min x y, max x y)

daysFrom :: Word16 -> Time -> [Time]
daysFrom n = take (fromIntegral n + 1) . iterate (+ 86400)

deriving instance (Show r, Arbitrary r, Testable (m a)) => Testable (ReaderT r m a)

noExceptionThrown :: Property
noExceptionThrown = property $ failed {reason = "Expected an exception but no exception was thrown"}

shouldThrowMatching :: MonadError e m => m a -> (e -> Bool) -> PropertyM m Property
shouldThrowMatching testOp errorMatcher = run $ failOnNoError testOp `catchError` (return . property . errorMatcher)
  where
    failOnNoError = (>> return noExceptionThrown)

throwingErrors :: (Exception e, Monad m) => PropertyM (ExceptT e m) a -> PropertyM m a
throwingErrors (MkPropertyM op) = MkPropertyM $
  \runTest -> hoistGen' ((handleUnexpected <$>) . runExceptT) $ op $ hoistGen' lift . runTest
  where
    handleUnexpected (Right p) = property p
    handleUnexpected (Left e) =
      property $ failed {reason = "Unexpected exception.", theException = Just (SomeException e)}

readerProp :: Monad m => r -> PropertyM (ReaderT r m) a -> PropertyM m a
readerProp r = hoistLiftedPropertyM (`runReaderT` r)

forAllEnvs :: (Arbitrary r, Show r, Monad m) => PropertyM (ReaderT r m) a -> PropertyM m a
forAllEnvs op = pick arbitrary >>= (readerProp ?? op)

hoistGen :: (forall x. n x -> m x) -> Gen (n a) -> Gen (m a)
hoistGen nat (MkGen f) = MkGen $ \gen -> nat . f gen

hoistGen' :: (n x -> m y) -> Gen (n x) -> Gen (m y)
hoistGen' nat (MkGen f) = MkGen $ \gen -> nat . f gen

hoistPropertyM :: (forall x. n x -> m x) -> (forall x. m x -> n x) -> PropertyM n a -> PropertyM m a
hoistPropertyM nat natInv (MkPropertyM f) = MkPropertyM $ \g -> hoistGen nat $ f $ hoistGen natInv . g

hoistLiftedPropertyM ::
  (MonadTrans t, Monad m, Coercible n (t m)) => (forall x. t m x -> m x) -> PropertyM n a -> PropertyM m a
hoistLiftedPropertyM lower = hoistPropertyM lower lift . coerce

appProp :: (HasCallStack, Testable a) => String -> PropertyM Gen a -> Spec
appProp desc = prop desc . monadic property

ignoreLogging :: Monad m => PropertyM (DiscardLoggingT msg m) a -> PropertyM m a
ignoreLogging = hoistLiftedPropertyM discardLogging

newtype TestM a = TestM {_runTest :: DiscardLoggingT LText (ExceptT App.Error (ReaderT App.Config Gen)) a}
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadReader App.Config,
      MonadError App.Error,
      MonadLog LText
    )

instance MonadOutcome ((,) EventOutcome) TestM where
  continue = return . (Continue,)
  stop = return . (Stop,)

data MockGraphiteResponses
  = MockResponses
      { _listMetricsResponse :: Either App.Error [Metric],
        _getMetricsResponse :: Either App.Error [DataPoint]
      }

makeLenses ''MockGraphiteResponses

type instance EventF TestM = (,) EventOutcome

instance MonadEventHandler e TestM where
  type EventS TestM = App.CurrentState
  handleEvent _ _ = TestM $ lift $ lift $ lift arbitrary2

instance MonadGraphite TestM where
  listMetrics = TestM $ lift $ lift $ lift arbitrary
  getMetrics _ = TestM $ lift $ lift $ lift arbitrary

instance GraphViewer TestM where
  updateGraph = TestM $ lift $ lift $ lift arbitrary

runMonadicTest :: Testable a => PropertyM TestM a -> Property
runMonadicTest =
  monadic
    ( \test -> property $ do
        env <- arbitrary
        result <- unwrapTest env test
        return (either unexpectedError property result)
    )
  where
    unwrapTest env = usingReaderT env . runExceptT . discardLogging . _runTest
    unexpectedError err = property $ failed {reason = "Unexpected exception.", theException = Just (SomeException err)}

assertAll :: (Foldable t, Monad m) => (a -> Bool) -> t a -> PropertyM m ()
assertAll predicate = assert . all predicate

activeState :: Gen App.CurrentState
activeState = arbitrary `suchThat` is App.active
