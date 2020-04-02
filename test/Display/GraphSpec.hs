{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Display.GraphSpec
  ( spec,
  )
where

import ArbitraryInstances ()
import Data.List
  ( maximum,
    minimum,
  )
import qualified Data.Set as S
import Display.Graph as Graph
import Graphite.Types
import Relude.Unsafe as Unsafe
import Test.Hspec as HS
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Prelude hiding (null)

genUniqueNonEmptyListSorted :: forall a. (Ord a, Arbitrary a) => Gen [a]
genUniqueNonEmptyListSorted = do
  distinct <- arbitrary @(Set a) `suchThat` (not . S.null)
  return $ sort . toList $ distinct

spec :: HS.Spec
spec = describe "Graph" $ do
  describe "Graphable (DataPoint)" . describe "extract" . prop "extracts graphable data from a type" $ \dp ->
    extract dp === (time dp, value dp)
  describe "bounds" . prop "correctly gets the bounds (Integers)" $ do
    xs <- genUniqueNonEmptyListSorted @Int
    ys <- vector @Int (length xs)
    let graph = mkGraph (xs `zip` ys)
    let (minX, maxX) = (Unsafe.head xs, Unsafe.last xs)
    let (minY, maxY) = (minimum ys, maximum ys)
    return $ boundsX graph === (minX, maxX) .&&. boundsY graph === (minY, maxY)
  describe "mkGraph" . prop "condenses duplicates correctly (last one wins)" $ \(a :: [(Int, Int)]) -> do
    (Positive i) <- arbitrary
    let duplicates = concat . transpose $ replicate i a
    return $ mkGraph a `shouldBe` mkGraph duplicates
  describe "mkGraph" . prop "does not remove elements that duplicate on the Y axis" $ \(y :: Int) -> do
    xs <- arbitrary @(Set Int)
    let points = (,y) <$> sort (toList xs)
    return $ assocs (mkGraph points) `shouldBe` points
  prop "order of input datapoints doesn't affect the equality of the graph" $
    \(xs :: [(Int, Int)]) -> forAll (shuffle xs) $ \shuffled -> mkGraph xs === mkGraph shuffled
