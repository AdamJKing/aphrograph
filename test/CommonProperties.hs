{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CommonProperties where

import           Test.QuickCheck
import           Test.QuickCheck.Property

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
            build 0 = return []
            build n' = do
                as <- build (n' - 1)
                a  <- arbitrary `suchThat` (`notElem` as)
                return $ a : as
