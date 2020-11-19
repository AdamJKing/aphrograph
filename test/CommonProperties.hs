{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module CommonProperties
  ( daysFrom,
    range,
    TestM,
    runTestM,
    FakeChanT,
  )
where

import App.Components (GraphViewer (..))
import qualified App.Config as App
import qualified App.State as App
import ArbitraryInstances ()
import Control.Monad.Base (MonadBase (..))
import Control.Monad.Except (MonadError, liftEither)
import Control.Monad.Trans.Control (MonadBaseControl (..))
import Display.GraphWidget (graphDisplayWidget)
import Events.Types (MonadEvent (..))
import Graphite.Types
  ( MonadGraphite (..),
    Time,
  )
import Test.Orphans ()
import Test.QuickCheck (Property, Testable (..), counterexample)
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary))
import Test.QuickCheck.Gen (Gen (..))
import Test.QuickCheck.GenT (MonadGen (liftGen), suchThat)
import Test.QuickCheck.Property (failed)
import Test.Tools (Lifted (..))

range :: (Ord a, Arbitrary a) => Gen (a, a)
range = do
  a <- arbitrary
  b <- arbitrary `suchThat` (/= a)
  return (min a b, max a b)

daysFrom :: Word16 -> Time -> [Time]
daysFrom n = take (fromIntegral n + 1) . iterate (+ 86400)

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

instance GraphViewer TestM where
  updateGraph ctx newGraph = return $ graphDisplayWidget ctx newGraph

newtype FakeChanT e m a = MkFakeChan (StateT [e] m a)
  deriving (Functor, Applicative, Monad, MonadState [e], MonadTrans)
  deriving (MonadGraphite) via (Lifted (FakeChanT e) m)

instance Monad m => MonadEvent e (FakeChanT e m) where
  writeEvent ev = modify (ev :)
  writeEventLater = (=<<) writeEvent
