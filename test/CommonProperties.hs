{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module CommonProperties
  ( appProp,
    daysFrom,
    range,
    getMetricsResponse,
    listMetricsResponse,
    assertAll,
    TestM,
    runTestM,
    FakeChanT,
    runFakeChan,
    inside,
    is,
  )
where

import App.Components (GraphViewer (..))
import qualified App.Config as App
import qualified App.State as App
import ArbitraryInstances ()
import Control.Lens (APrism', Getting, matching, (^?))
import Control.Lens.TH (makeLenses)
import Control.Monad.Base (MonadBase (..))
import Control.Monad.Except (MonadError, liftEither)
import Control.Monad.Trans.Control (MonadBaseControl (..))
import Display.GraphWidget (graphDisplayWidget)
import Events.Types (MonadEvent (..))
import Graphite.Types
  ( DataPoint,
    Metric,
    MonadGraphite (..),
    Time,
  )
import Test.Hspec (Spec)
import Test.Hspec.QuickCheck (prop)
import Test.Orphans ()
import Test.QuickCheck (Property, Testable (..), counterexample)
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary))
import Test.QuickCheck.Gen (Gen (..))
import Test.QuickCheck.GenT (MonadGen (liftGen), suchThat)
import Test.QuickCheck.Monadic (PropertyM, assert, monadic)
import Test.QuickCheck.Property (failed, succeeded)
import Test.Tools (Lifted (..))

range :: (Ord a, Arbitrary a) => Gen (a, a)
range = do
  a <- arbitrary
  b <- arbitrary `suchThat` (/= a)
  return (min a b, max a b)

daysFrom :: Word16 -> Time -> [Time]
daysFrom n = take (fromIntegral n + 1) . iterate (+ 86400)

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
eitherProp (Left err) = counterexample (show err) $ property failed

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

data MockGraphiteResponses = MockResponses
  { _listMetricsResponse :: Either App.Error [Metric],
    _getMetricsResponse :: Either App.Error [DataPoint]
  }

makeLenses ''MockGraphiteResponses

instance MonadGraphite TestM where
  listMetrics = liftGen arbitrary
  getMetrics _ = liftGen arbitrary

instance GraphViewer TestM where
  updateGraph ctx newGraph = return $ graphDisplayWidget ctx newGraph

assertAll :: (Foldable t, Monad m) => (a -> Bool) -> t a -> PropertyM m ()
assertAll predicate = assert . all predicate

newtype FakeChanT e m a = MkFakeChan {unFakeChan :: StateT [e] m a}
  deriving (Functor, Applicative, Monad, MonadState [e], MonadTrans)
  deriving (MonadGraphite) via (Lifted (FakeChanT e) m)

runFakeChan :: FakeChanT e m a -> m (a, [e])
runFakeChan = usingStateT [] . unFakeChan

instance Monad m => MonadEvent e (FakeChanT e m) where
  writeEvent ev = modify (ev :)
  writeEventLater = (=<<) writeEvent

is :: Show s => s -> ([Char], APrism' s a) -> Property
is target (desc, prism) = case matching prism target of
  Right _ -> property succeeded
  Left unexpected -> counterexample (show unexpected ++ " was not a " ++ desc) (property failed)

inside :: a -> String -> Getting (First t) a t -> (t -> Property) -> Property
inside outer desc getter condition =
  case outer ^? getter of
    Just inner -> counterexample (desc ++ " did not match condition") (condition inner)
    Nothing -> counterexample ("Did not find " ++ desc) (property failed)
