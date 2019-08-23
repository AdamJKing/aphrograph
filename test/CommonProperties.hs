{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

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
import           Test.QuickCheck.Monadic
import           Test.Hspec.QuickCheck          ( prop )
import           Control.Monad.Except           ( ExceptT
                                                , MonadError
                                                )

range :: (Ord a, Arbitrary a) => Gen (a, a)
range = do
  x <- arbitrary
  y <- arbitrary `suchThat` (/= x)
  return (min x y, max x y)

daysFrom :: Word16 -> Time -> [Time]
daysFrom n = take (fromIntegral n + 1) . iterate (+ 86400)

deriving instance ( Show r, Arbitrary r, Testable (m a)) => Testable (ReaderT r m a)

-- instance (Functor m, Testable prop, Testable (m Property), Exception e) => Testable (ExceptT e m prop) where
instance (Functor m, forall p. Testable p => Testable (m p), Testable prop, Exception e) => Testable (ExceptT e m prop) where
  property op = property $ runExceptT op <&> \case
    Right outcome -> property outcome
    Left  failure -> property $ failed
      { reason = "Unexpected exception: " ++ displayException failure
      }

newtype MockApp a = MockApp { _unApp :: ReaderT ActiveState (ExceptT AppError Gen ) a }
  deriving (Functor, Applicative, Monad, Testable, MonadReader ActiveState, MonadError AppError)
  deriving (MonadLog msg) via (DiscardLoggingT msg MockApp)

instance MonadGraphite MockApp where
  listMetrics = MockApp . lift $ lift arbitrary
  getMetrics _ = MockApp . lift $ lift arbitrary

mockApp :: Testable a => PropertyM MockApp a -> Gen (MockApp Property)
mockApp = monadic'

appProp :: Testable a => String -> PropertyM MockApp a -> Spec
appProp desc = prop desc . mockApp
