{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Display.GraphSpec where

import           Display.Graph                 as Graph
import           CommonProperties
import           ArbitraryInstances             ( )
import           Test.Hspec                    as HS
import           Test.QuickCheck
import           Graphite.Types                 ( time
                                                , value
                                                )
import           Test.Hspec.QuickCheck
import           Relude.Unsafe                 as Unsafe
import           Data.List                      ( maximum
                                                , minimum
                                                )

spec :: HS.Spec
spec = describe "Graph" $ do
    describe "Graphable (DataPoint)"
        . describe "extract"
        . prop "extracts graphable data from a type"
        $ \dp -> extract dp === (time dp, value dp)

    describe "boundsX"
        . prop "correctly gets the bounds (Integers)"
        $ \((Unique xs) :: UniqueList Int) ((Unique ys) :: UniqueList Int) ->
              (not (null xs) && not (null ys))
                  ==> (length xs == length ys)
                  ==> let graph        = mkGraph (xs `zip` ys)
                          (minX, maxX) = (minimum xs, maximum xs)
                      in  boundsX graph === (minX, maxX)

    describe "boundsY"
        . prop "correctly gets the bounds (Integers)"
        $ \((Unique xs) :: UniqueList Int) ((Unique ys) :: UniqueList Int) ->
              (not (null xs) && not (null ys))
                  ==> (length xs == length ys)
                  ==> let graph        = mkGraph (xs `zip` ys)
                          (minY, maxY) = (minimum ys, maximum ys)
                      in  boundsY graph === (minY, maxY)

    describe "mapPoints"
        . prop "mapping with identity doesn't change the map"
        $ \(graph :: Graph Int Int) -> mapPoints id graph === graph

    describe "mapPoints"
        . prop "condenses duplicates correctly"
        $ \(graph :: Graph Int Int) -> do
              dupPoint <- Unsafe.head <$> shuffle (assocs graph)
              let outcome  = mapPoints (const dupPoint) graph
              let expected = mkGraph [dupPoint]
              return $ outcome `shouldBe` expected

    describe "mkGraph"
        . prop "condenses duplicates correctly (last one wins)"
        $ \(a :: [(Int, Int)]) -> do
              (Positive i) <- arbitrary
              let duplicates = concat . transpose $ replicate i a
              return $ mkGraph a `shouldBe` mkGraph duplicates

    describe "mkGraph"
        . prop "does not remove elements that duplicate on the Y axis"
        $ \(y :: Int) -> do
              (Unique (xs :: [Int])) <- arbitrary
              let points = (, y) <$> sort xs
              return $ assocs (mkGraph points) `shouldBe` points

    describe "member"
        . prop "correctly identifies members of a graph"
        $ \(Unique xs) -> do
              ys <- vectorOf (length xs) arbitrary
              let points = xs `zip` ys
              let graph  = mkGraph points :: Graph Int Int
              return $ all (`member` graph) points

    describe "member"
        . prop "correctly idenitifies non-members of a graph"
        $ \(Unique xs) -> do
              ys <- vectorOf (length xs) arbitrary
              let graph = mkGraph (xs `zip` ys) :: Graph Int Int
              return
                  $ forAll (arbitrary `suchThat` (`notElem` (assocs graph)))
                  $ not
                  . (`member` graph)
