{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module CommonProperties
  ( shouldThrowMatching
  , withMockGraphite
  , forAllEnvs
  , throwingErrors
  , ignoreLogging
  , appProp
  , daysFrom
  , range
  )
where

import           ArbitraryInstances             ( )
import           Test.QuickCheck.Property       ( Testable(..)
                                                , Property
                                                , Result(..)
                                                , failed
                                                )
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen            ( Gen(..) )
import           Test.QuickCheck.GenT
import           Test.Hspec
import           Graphite.Types
import           App.State                     as App
import           Test.QuickCheck.Monadic
import           Test.Hspec.QuickCheck          ( prop )
import           Control.Monad.Except           ( MonadError(catchError) )
import           Data.Time.LocalTime            ( utc )
import           Control.Monad.Log

range :: (Ord a, Arbitrary a) => Gen (a, a)
range = do
  x <- liftGen arbitrary
  y <- liftGen arbitrary `suchThat` (/= x)
  return (min x y, max x y)

daysFrom :: Word16 -> Time -> [Time]
daysFrom n = take (fromIntegral n + 1) . iterate (+ 86400)

deriving instance ( Show r, Arbitrary r, Testable (m a)) => Testable (ReaderT r m a)

noExceptionThrown :: Property
noExceptionThrown = property $ failed { reason = "Expected an exception but no exception was thrown" }

shouldThrowMatching :: MonadError e m => m a -> (e -> Property) -> PropertyM m Property
shouldThrowMatching testOp errorMatcher = run $ failOnNoError testOp `catchError` (return . property . errorMatcher)
  where failOnNoError = (>> return noExceptionThrown)

newtype EmptyState s = Empty s

instance Arbitrary (EmptyState ActiveState) where
  arbitrary = return . Empty $ ActiveState { _metricsView = Nothing, _graphData = mempty, _timezone = utc }

throwingErrors :: (Exception e, Monad m) => PropertyM (ExceptT e m) a -> PropertyM m a
throwingErrors (MkPropertyM op) = MkPropertyM
  $ \runTest -> hoistGen' ((handleUnexpected <$>) . runExceptT) $ op $ hoistGen' lift . runTest
 where
  handleUnexpected (Right p) = property p
  handleUnexpected (Left e) =
    property $ failed { reason = "Unexpected exception.", theException = Just (SomeException e) }

readerProp :: Monad m => r -> PropertyM (ReaderT r m) a -> PropertyM m a
readerProp r = hoistLiftedPropertyM (`runReaderT` r)

forAllEnvs :: (Arbitrary r, Show r, Monad m) => PropertyM (ReaderT r m) a -> PropertyM m a
forAllEnvs op = pick arbitrary >>= (readerProp ?? op)

hoistGen :: (forall x . n x -> m x) -> Gen (n a) -> Gen (m a)
hoistGen nat (MkGen f) = MkGen $ \gen -> nat . f gen

hoistGen' :: (n x -> m y) -> Gen (n x) -> Gen (m y)
hoistGen' nat (MkGen f) = MkGen $ \gen -> nat . f gen

hoistPropertyM :: (forall x . n x -> m x) -> (forall x . m x -> n x) -> PropertyM n a -> PropertyM m a
hoistPropertyM nat natInv (MkPropertyM f) = MkPropertyM $ \g -> hoistGen nat $ f $ hoistGen natInv . g

hoistLiftedPropertyM
  :: (MonadTrans t, Monad m, Coercible n (t m)) => (forall x . t m x -> m x) -> PropertyM n a -> PropertyM m a
hoistLiftedPropertyM lower = hoistPropertyM lower lift . coerce

appProp :: (HasCallStack, Testable a) => String -> PropertyM Gen a -> Spec
appProp desc = prop desc . monadic property

ignoreLogging :: Monad m => PropertyM (DiscardLoggingT msg m) a -> PropertyM m a
ignoreLogging = hoistLiftedPropertyM discardLogging

newtype ArbitraryGraphite m a = ArbitraryGraphite { _run :: StateT ([Metric], [DataPoint]) m a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadTrans
           , MonadState ([Metric], [DataPoint])
           , MonadError e
           , MonadLog log
           , MonadReader r)

withMockGraphite :: MonadGen m => PropertyM (ArbitraryGraphite m) a -> PropertyM m a
withMockGraphite = hoistLiftedPropertyM $ \op -> do
  a           <- liftGen arbitrary
  b           <- liftGen arbitrary
  (result, _) <- runStateT (_run op) (a, b)
  return result

instance Monad m => MonadGraphite ( ArbitraryGraphite m ) where
  listMetrics = fst <$> get
  getMetrics _ = snd <$> get

