{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}

module Display.GraphSpec where

import           Display.Graph                 as Graph
import           ArbitraryInstances             ( )
import           Test.Hspec                    as HS
import           Prelude                 hiding ( null )
import           Test.QuickCheck
import           Graphite.Types
import qualified Data.Set                      as S
import           Test.Hspec.QuickCheck
import           Relude.Unsafe                 as Unsafe
import           Data.List                      ( maximum
                                                , minimum
                                                )

genUniqueNonEmptyListSorted :: forall  a . (Ord a, Arbitrary a) => Gen [a]
genUniqueNonEmptyListSorted = do
  distinct <- arbitrary @(Set a) `suchThat` (not . S.null)
  return $ sort . toList $ distinct

uniqueFstTupleList' :: forall  a b . (Ord a, Arbitrary a, Arbitrary b) => Gen (SortedList (a, b))
uniqueFstTupleList' = do
  distinct <- arbitrary @(Set a)
  values   <- arbitrary @(InfiniteList b)
  return . Sorted $ toList distinct `zip` getInfiniteList values


spec :: HS.Spec
spec = describe "Graph" $ do
  describe "extractGraph" $ do
    prop "extracting a graph is equivalent to creating one from a list of points" $ \(points :: [DataPoint]) ->
      Graph.mkGraph (Graph.extract <$> points) === (Graph.extractGraph points :: Graph Time Value)

    prop "graph extracted from list has the same elements"
      $ forAll uniqueFstTupleList'
      $ \(Sorted (points :: [(Int, Int)])) -> points `shouldSatisfy` all (`member` Graph.mkGraph points)

  describe "Graphable (DataPoint)" . describe "extract" . prop "extracts graphable data from a type" $ \dp ->
    extract dp === (time dp, value dp)

  describe "bounds" . prop "correctly gets the bounds (Integers)" $ do
    xs <- genUniqueNonEmptyListSorted @Int
    ys <- vector @Int (length xs)
    let graph        = mkGraph (xs `zip` ys)
    let (minX, maxX) = (Unsafe.head xs, Unsafe.last xs)
    let (minY, maxY) = (minimum ys, maximum ys)
    return $ boundsX graph === (minX, maxX) .&&. boundsY graph === (minY, maxY)


  describe "mapPoints" . prop "mapping with identity doesn't change the map" $ \(graph :: Graph Int Int) ->
    mapPoints id graph === graph

  describe "mapPoints"
    . prop "condenses duplicates correctly"
    . forAll (arbitrary `suchThat` (not . null))
    $ \(graph :: Graph Int Int) -> do
        dupPoint <- Unsafe.head <$> shuffle (assocs graph)
        let outcome  = mapPoints (const dupPoint) graph
        let expected = mkGraph [dupPoint]
        return $ outcome `shouldBe` expected

  describe "mkGraph" . prop "condenses duplicates correctly (last one wins)" $ \(a :: [(Int, Int)]) -> do
    (Positive i) <- arbitrary
    let duplicates = concat . transpose $ replicate i a
    return $ mkGraph a `shouldBe` mkGraph duplicates

  describe "mkGraph" . prop "does not remove elements that duplicate on the Y axis" $ \(y :: Int) -> do
    xs <- arbitrary @(Set Int)
    let points = (, y) <$> sort (toList xs)
    return $ assocs (mkGraph points) `shouldBe` points

  describe "member" . prop "correctly identifies members of a graph" $ \(xs :: Set Int) -> do
    ys <- vectorOf (length xs) arbitrary
    let points = toList xs `zip` ys
    let graph  = mkGraph points :: Graph Int Int
    return $ all (`member` graph) points

  describe "member" . prop "correctly idenitifies non-members of a graph" $ \(xs :: [Int]) -> do
    ys <- vectorOf (length xs) arbitrary
    let graph = mkGraph (xs `zip` ys) :: Graph Int Int
    return $ forAll (arbitrary `suchThat` (`notElem` assocs graph)) $ not . (`member` graph)
