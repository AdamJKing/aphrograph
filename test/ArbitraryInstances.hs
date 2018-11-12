{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ArbitraryInstances where

import           Display.Graph
import           Data.Bifunctor
import           System.Random
import           Test.QuickCheck
import           Display.Types
import           Time.Types
import           Graphite

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

newtype SimpleElapsed = SimpleElapsed Elapsed deriving (Eq, Show)
newtype SimpleDataPoint = SimpleDataPoint DataPoint deriving (Eq, Show, Ord)
newtype SimpleGraph = SimpleGraph (Graph Int Int) deriving (Show, Eq)

instance Arbitrary SimpleDataPoint where
    arbitrary = SimpleDataPoint <$> do
        s <- arbitrary
        (SimpleElapsed e) <- arbitrary
        return $ DataPoint s e

    shrink (SimpleDataPoint DataPoint{value=v, time=t}) = do
        v' <- shrink v
        (SimpleElapsed t') <-  shrink (SimpleElapsed t)
        return . SimpleDataPoint $  DataPoint v' t'

instance Random SimpleSeconds where
    randomR rng gen = first (SimpleSeconds . Seconds . abs) $ randomR (asInt rng) gen
        where asInt (SimpleSeconds (Seconds lower), SimpleSeconds (Seconds higher)) = (lower, higher)

    random gen = first (SimpleSeconds . Seconds . abs) $ random gen

instance Random SimpleElapsed where
    randomR rng gen = first (SimpleElapsed . Elapsed . get) $ randomR (asSeconds rng) gen
        where
            get (SimpleSeconds s) = s
            asSeconds (SimpleElapsed (Elapsed lower), SimpleElapsed (Elapsed higher)) = (SimpleSeconds lower, SimpleSeconds higher)

    random gen = first (SimpleElapsed . Elapsed . get) $ random gen
            where get (SimpleSeconds s) = s

newtype SimpleDimensions = SimpleDimensions (Dimensions Int)

instance Arbitrary SimpleDimensions where
    arbitrary = SimpleDimensions <$> (Dimensions <$> arbitrary <*> arbitrary)
    shrink (SimpleDimensions Dimensions{..}) = do
        w <- shrink width
        h <- shrink height
        return . SimpleDimensions $ Dimensions w h

instance Arbitrary SimpleGraph where
    arbitrary = do
        (NonEmpty xs) <- arbitrary
        (InfiniteList ys _) <- arbitrary
        return . SimpleGraph . mkGraph $ xs `zip` ys

    shrink (SimpleGraph graph) = SimpleGraph . mkGraph <$> shrink (assocs graph)

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
