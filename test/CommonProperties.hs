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

module CommonProperties where

import           ArbitraryInstances             ( )
import           Test.QuickCheck.Property
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen
import           Test.Hspec
import           Graphite.Types
import           Control.Monad.Log
import           App.State                     as App
import           Test.QuickCheck.Monadic
import           App
import           Test.Hspec.QuickCheck          ( prop )
import           Control.Monad.Except           ( MonadError(catchError) )
import           Control.Lens.Extras            ( is )
import           Data.Time.LocalTime            ( utc )
import           Control.Lens.Prism

range :: (Ord a, Arbitrary a) => Gen (a, a)
range = do
  x <- arbitrary
  y <- arbitrary `suchThat` (/= x)
  return (min x y, max x y)

daysFrom :: Word16 -> Time -> [Time]
daysFrom n = take (fromIntegral n + 1) . iterate (+ 86400)

deriving instance ( Show r, Arbitrary r, Testable (m a)) => Testable (ReaderT r m a)

instance (Monad m, Testable (m Property), Testable prop, Exception e) => Testable (ExceptT e m prop) where
  property = property . monadic' . lift . runExceptT

newtype MockApp s a = MockApp { _unApp :: ReaderT s Gen a }
  deriving (Functor, Applicative, Monad, Testable, MonadReader s)
  deriving (MonadLog msg) via (DiscardLoggingT msg ( MockApp s ))

instance MonadGraphite ( MockApp s ) where
  listMetrics = MockApp . lift $ arbitrary
  getMetrics _ = MockApp . lift $ arbitrary

mockApp :: Testable a => PropertyM (MockApp ActiveState) a -> Gen (MockApp ActiveState Property)
mockApp = monadic'

appProp :: (HasCallStack, Testable a) => String -> PropertyM (MockApp ActiveState) a -> Spec
appProp desc = prop desc . mockApp

noExceptionThrown :: Property
noExceptionThrown = property $ failed { reason = "Expected an exception but no exception was thrown" }

-- shouldThrow :: (Show e, Eq e, MonadError e m) => m a -> e -> PropertyM m Property
-- shouldThrow testOp expectedError = testOp `shouldThrowMatching` (=== expectedError)

-- shouldThrowMatching :: MonadError e m => m a -> (e -> Property) -> PropertyM m Property
-- shouldThrowMatching testOp errorMatcher = run $ failOnNoError testOp `catchError` (return . errorMatcher)
--   where failOnNoError = (>> return noExceptionThrown)

shouldThrowMatching :: MonadError e m => m a -> APrism' e a -> PropertyM m Property
shouldThrowMatching testOp errorMatcher = run $ failOnNoError testOp `catchError` (return . property . is errorMatcher)
  where failOnNoError = (>> return noExceptionThrown)

newtype EmptyState s = Empty { unEmpty :: s }

instance Arbitrary (EmptyState ActiveState) where
  arbitrary = return . Empty $ ActiveState { _metricsView = Nothing, _graphData = mempty, _timezone = utc }

monadicApp :: (Monad m, Testable t) => PropertyM (AppT m) t -> Gen (AppT m Property)
monadicApp = monadic'

instance (Testable prop, Exception e) => Testable (Either e prop) where
  property (Right outcome) = property outcome
  property (Left  failure) = property $ failed { reason = "Unexpected exception: " ++ displayException failure }

newtype Test a = Test a
