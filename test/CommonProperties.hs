{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module CommonProperties where

import qualified Brick.Types                    as Brick
import qualified Test.QuickCheck.GenT          as Gen
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
import           Data.Typeable
import           Data.Time.LocalTime            ( utc )

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

unexpectedException :: Exception e => e -> Property
unexpectedException err =
  property $ failed { reason = "Unexpected exception was thrown", theException = Just (SomeException err) }

shouldThrow :: (Testable prop, MonadError e m, Exception e) => m a -> (e -> m prop) -> PropertyM m Property
shouldThrow test matcher = run $ failOnNoError test `catchError` identifyError
 where
  failOnNoError = (>> return (property noExceptionThrown))
  identifyError thrownError = case cast thrownError of
    Just identifiedError -> property <$> matcher identifiedError
    Nothing              -> return (unexpectedException thrownError)

newtype EmptyState s = Empty { unEmpty :: s }

instance Arbitrary (EmptyState ActiveState) where
  arbitrary = return . Empty $ ActiveState { _metricsView = Nothing, _graphData = mempty, _timezone = utc }

monadicApp :: Testable t => PropertyM AppM t -> Gen (AppM Property)
monadicApp = monadic'

instance (Testable prop, Exception e) => Testable (Either e prop) where
  property (Right outcome) = property outcome
  property (Left  failure) = property $ failed { reason = "Unexpected exception: " ++ displayException failure }

instance Gen.MonadGen ( Brick.EventM n ) where

instance Testable t => Testable (AppM t) where
  property testApp = monadic' (run testApp)
    -- let noOpLogger = (return . pass) in property (\conf -> idempotentIOProperty $ runApp noOpLogger conf testApp)
