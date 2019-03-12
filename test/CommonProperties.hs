{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module CommonProperties where

import           Test.QuickCheck
import           Test.QuickCheck.Property
import           Test.Hspec

ofEither :: (Testable t, Show b) => (a -> t) -> Either b a -> Property
ofEither assert target = case target of
    Right value -> property $ assert value
    Left  other -> counterexample
        ("Expected Right, but found Left of " ++ show other)
        (property failed)

newtype UniqueList a = Unique {
    getUnique :: [a]
} deriving (Show, Eq, Foldable)

instance (Arbitrary a, Eq a) => Arbitrary (UniqueList a) where
    arbitrary = sized $ fmap Unique . build
      where
        build 0  = return []
        build n' = do
            as <- build (n' - 1)
            a  <- arbitrary `suchThat` (`notElem` as)
            return $ a : as

shouldBeM :: (Eq a, Show a, Functor f) => f a -> a -> f Expectation
shouldBeM op expected = (`shouldBe` expected) <$> op

range :: (Ord a, Arbitrary a) => Gen (a, a)
range = do
    x <- arbitrary
    y <- arbitrary `suchThat` (/= x)
    return (min x y, max x y)
