{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module GraphiteMock where

import ArbitraryInstances ()
import Graphite.Types (MonadGraphite (..))
import Test.QuickCheck (Arbitrary (arbitrary))
import Test.QuickCheck.GenT (GenT, MonadGen, liftGen, runGenT)
import Test.QuickCheck.Instances.Vector ()

newtype GraphiteMock m a = GM (GenT m a)
  deriving (Applicative, Functor, Monad, MonadTrans, MonadGen)

instance Monad m => MonadGraphite (GraphiteMock m) where
  listMetrics = liftGen arbitrary
  getMetrics _ = liftGen arbitrary

withMockGraphite :: MonadGen g => GraphiteMock m a -> g (m a)
withMockGraphite (GM test) = liftGen (runGenT test)