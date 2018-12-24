{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingVia #-}

module ArbitraryInstances where

import           Labels
import           Data.Decimal
import           Display.Graph
import           System.Random
import           Test.QuickCheck
import           Display.Types
import           Time.Types
import           Graphite
import           Relude.Extra.Newtype
import           Test.Types

instance Arbitrary SimpleSeconds where
    arbitrary = SimpleSeconds <$> do
        Positive x <- arbitrary
        return $ Seconds x

    shrink (SimpleSeconds (Seconds s)) = SimpleSeconds . Seconds <$> shrink s

instance Arbitrary SimpleElapsed where
    arbitrary = SimpleElapsed <$> do
        (SimpleSeconds seconds) <- arbitrary
        return $ Elapsed seconds

newtype SimpleSeconds = SimpleSeconds {
    getSeconds :: Seconds
} deriving (Eq, Show, Num, Ord)

newtype SimpleElapsed = SimpleElapsed {
    getElapsed :: Elapsed
} deriving (Eq, Show, Num, Ord)

newtype SimpleDataPoint = SimpleDataPoint DataPoint deriving (Eq, Show, Ord)
newtype SimpleGraph = SimpleGraph (Graph Int Int) deriving (Show, Eq)

newtype TestDiscreteValue = TestDiscrete (DiscreteValue Int) deriving (Show, Eq)

instance Arbitrary TestDiscreteValue where
    arbitrary = TestDiscrete . Discrete <$> arbitrary

newtype NonZeroTestDiscreteValue = NonZeroTestDiscrete (DiscreteValue Int) deriving (Show, Eq)

instance Arbitrary NonZeroTestDiscreteValue where
    arbitrary = NonZeroTestDiscrete . Discrete <$> (arbitrary `suchThat` (> 0))

newtype TestDecimal = TestDecimal Decimal deriving (Show, Eq, Num)

instance Arbitrary TestDecimal where
    arbitrary = do
        (Positive a) <- arbitrary
        b            <- choose (0, 10)
        return . TestDecimal $ Decimal a b


newtype NonZeroTestDecimal = NonZeroTestDecimal Decimal deriving (Show, Eq)

instance Arbitrary NonZeroTestDecimal where
    arbitrary = do
        (NonZero (TestDecimal d)) <- arbitrary
        return . NonZeroTestDecimal $ d

instance Arbitrary SimpleDataPoint where
    arbitrary = SimpleDataPoint <$> do
        (TestValue     s) <- arbitrary
        (SimpleElapsed e) <- arbitrary
        return $ DataPoint s (Time e)

    shrink (SimpleDataPoint DataPoint { value = v, time = (Time t) }) = do
        (TestValue     v') <- shrink (TestValue v)
        (SimpleElapsed t') <- shrink (SimpleElapsed t)
        return . SimpleDataPoint $ DataPoint v' (Time t')

instance Random SimpleSeconds where
    randomR rng gen = first (SimpleSeconds . Seconds . abs)
        $ randomR (asInt rng) gen
      where
        asInt (SimpleSeconds (Seconds lower), SimpleSeconds (Seconds higher)) =
            (lower, higher)

    random gen = first (SimpleSeconds . Seconds . abs) $ random gen

instance Random SimpleElapsed where
    randomR rng gen = first (SimpleElapsed . Elapsed . un)
        $ randomR (asSeconds rng) gen
      where
        asSeconds (SimpleElapsed (Elapsed lower), SimpleElapsed (Elapsed higher))
            = (SimpleSeconds lower, SimpleSeconds higher)

    random gen = first (SimpleElapsed . Elapsed . get') $ random gen
        where get' (SimpleElapsed (Elapsed s)) = s

newtype SimpleDimensions = SimpleDimensions (Dimensions Int)

instance Arbitrary SimpleDimensions where
    arbitrary = SimpleDimensions <$> (Dimensions <$> arbitrary <*> arbitrary)
    shrink (SimpleDimensions Dimensions {..}) = do
        w <- shrink width
        h <- shrink height
        return . SimpleDimensions $ Dimensions w h

instance Arbitrary SimpleGraph where
    arbitrary = do
        (NonEmpty xs      ) <- arbitrary
        (InfiniteList ys _) <- arbitrary
        return . SimpleGraph . mkGraph $ xs `zip` ys

    shrink (SimpleGraph graph) =
        SimpleGraph . mkGraph <$> shrink (assocs graph)

data Range i = Range {
    lower :: i,
    higher :: i
} deriving (Show, Eq)

tuple :: Range n -> (n, n)
tuple Range {..} = (lower, higher)

instance (Ord a, Arbitrary a) => Arbitrary (Range a) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary `suchThat` (/=) x
        return $ Range { lower = min x y, higher = max x y }

genTestData
    :: (Arbitrary a, Random a, Ord a, Num a) => Gen (Range a, Positive a)
genTestData = do
    r@Range {..} <- arbitrary
    x            <- choose (lower, higher)
    return (r, Positive x)
