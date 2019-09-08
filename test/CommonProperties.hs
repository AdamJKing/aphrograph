{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module CommonProperties where

import           ArbitraryInstances             ( )
import           Test.QuickCheck.Property
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen
import           Test.Hspec
import           Graphite.Types
import           Control.Monad.Log
import           App
import           App.Args                      as App
import           Test.QuickCheck.Monadic
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

instance (Functor m, Testable (m Property), Testable prop, Exception e) => Testable (ExceptT e m prop) where
  property op = property $ runExceptT op <&> \case
    Right outcome -> property outcome
    Left failure ->
      property $ failed { reason = "Unexpected exception: " ++ displayException failure }

newtype MockApp s a = MockApp { _unApp :: ReaderT s (ExceptT AppError Gen ) a }
  deriving (Functor, Applicative, Monad, Testable, MonadReader s, MonadError AppError)
  deriving (MonadLog msg) via (DiscardLoggingT msg ( MockApp s ))

instance MonadGraphite ( MockApp s ) where
  listMetrics = MockApp . lift $ lift arbitrary
  getMetrics _ = MockApp . lift $ lift arbitrary

mockApp
  :: Testable a => PropertyM (MockApp ActiveState) a -> Gen (MockApp ActiveState Property)
mockApp = monadic'

appProp
  :: (HasCallStack, Testable a) => String -> PropertyM (MockApp ActiveState) a -> Spec
appProp desc = prop desc . mockApp

appProp' :: Testable a => String -> AppM ActiveState Gen a -> Spec
appProp' = prop

runAppProperty :: AppM ActiveState Gen Property -> Property
runAppProperty = property

instance ( Arbitrary s, Show s, Testable (m a), Functor m ) => Testable (AppM s m a) where
  property = property . flip runAppM

noExceptionThrown :: Property
noExceptionThrown =
  property $ failed { reason = "Expected an exception but no exception was thrown" }

unexpectedException :: Exception e => e -> Property
unexpectedException err = property $ failed { reason = "Unexpected exception was thrown"
                                            , theException = Just (SomeException err)
                                            }

shouldThrow
  :: (Testable prop, MonadError e m, Exception e)
  => m a
  -> (e -> m prop)
  -> PropertyM m Property
shouldThrow test matcher = run $ failOnNoError test `catchError` identifyError
 where
  failOnNoError = (>> return (property noExceptionThrown))
  identifyError thrownError = case cast thrownError of
    Just identifiedError -> property <$> matcher identifiedError
    Nothing              -> return (unexpectedException thrownError)

newtype EmptyState s = Empty { unEmpty :: s }

instance Arbitrary (EmptyState ActiveState) where
  arbitrary = arbitrary @App.Args >>= \args -> return . Empty $ ActiveState
    { _metricsView = Nothing
    , _graphData   = mempty
    , _timezone    = Just utc
    , _appArgs     = args
    , _logger      = const pass
    }
