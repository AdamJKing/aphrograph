{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module CommonProperties where

import           ArbitraryInstances             ( )
import           Graphite
import           Test.QuickCheck
import           Graphite.Types
import           App.Args                      as App
import           Control.Monad.Log


newtype UniqueList a = Unique { getUnique :: [a] }
  deriving (Show, Eq, Foldable)

instance (Arbitrary a, Eq a) => Arbitrary (UniqueList a) where
  arbitrary = sized $ fmap Unique . build
   where
    build 0  = return []
    build n' = do
      as <- build (n' - 1)
      a  <- arbitrary `suchThat` (`notElem` as)
      return $ a : as

range :: (Ord a, Arbitrary a) => Gen (a, a)
range = do
  x <- arbitrary
  y <- arbitrary `suchThat` (/= x)
  return (min x y, max x y)

-- Temporary until QuickCheck 2.13
instance Testable prop => Testable (Maybe prop) where
  property = property . liftMaybe
   where
    liftMaybe Nothing     = property Discard
    liftMaybe (Just prop) = property prop

daysFrom :: Word16 -> Time -> [Time]
daysFrom n = take (fromIntegral n + 1) . iterate (+ 86400)

data DummyComponent = DummyComponent deriving (Eq, Show)

newtype TestIO a = TestIO (ReaderT App.Args (DiscardLoggingT Text IO) a)
    deriving (Functor, Applicative, Monad, MonadLog Text, MonadReader App.Args, MonadIO)

arbitraryTestIO :: (Arbitrary a) => TestIO a
arbitraryTestIO = liftIO $ generate arbitrary

instance (Testable t) => Testable ( TestIO t ) where
  property (TestIO t) = idempotentIOProperty $ do
    args <- generate (applyArbitrary4 App.Args)
    discardLogging $ usingReaderT args t

data FailingGraphiteMock a = FailingGraphiteMock a deriving (Applicative, Functor, Monad )

instance MonadGraphite FailingGraphiteMock where
  getMetrics _ = error "Something bad!"
