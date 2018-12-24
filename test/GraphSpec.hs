{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module GraphSpec where

import           Display.Graph                 as Graph

import           Normalisation                  ( NormalisationFailure(..) )
import           CommonProperties
import           Test.Hspec                    as HS

import           Test.QuickCheck
import           Graphite
import           ArbitraryInstances
import           Relude.Unsafe                 as Unsafe
import           Test.Types
import           Relude.Extra.Newtype
import           Data.List                      ( maximum
                                                , minimum
                                                )

spec :: HS.Spec
spec = describe "Graph" $ do
    describe "Graphable (DataPoint)"
        . describe "extract"
        . it "extracts graphable data from a type"
        . property
        $ \(SimpleDataPoint dp) -> extract dp === (time dp, value dp)

    describe "boundsX"
        . it "correctly gets the bounds (Integers)"
        . property
        $ \((Unique xs) :: UniqueList Int) ((Unique ys) :: UniqueList Int) ->
              (not (null xs) && not (null ys))
                  ==> (length xs == length ys)
                  ==> let graph        = mkGraph (xs `zip` ys)
                          (minX, maxX) = (minimum xs, maximum xs)
                      in  boundsX graph === (minX, maxX)

    describe "boundsY"
        . it "correctly gets the bounds (Integers)"
        . property
        $ \((Unique xs) :: UniqueList Int) ((Unique ys) :: UniqueList Int) ->
              (not (null xs) && not (null ys))
                  ==> (length xs == length ys)
                  ==> let graph        = mkGraph (xs `zip` ys)
                          (minY, maxY) = (minimum ys, maximum ys)
                      in  boundsY graph === (minY, maxY)

    describe "Scaled (Double to Integer)"
        . describe "scaling a value"
        . it "scales to a value in the target range"
        . property
        $ \(from :: Range TestValue) (to :: Range Integer) -> do
              (TestValue v) <- choose (lower from, higher from)
              let outcome = Graph.scale v
                                        (un $ lower from, un $ higher from)
                                        (lower to       , higher to)
              return . counterexample (show (v, outcome)) $ ofEither
                  (\a -> (lower to <= a) && a <= higher to)
                  outcome

    describe "Scaled (Time to Integer)"
        . describe "scaling a value"
        . it "scales to a value in the target range"
        . property
        $ \(Range l h) (Range l' h') -> do
              (TestTime v) <- choose (l, h) :: Gen TestTime
              let outcome :: Either NormalisationFailure Integer
                  outcome = Graph.scale v (un l, un h) (l', h')
              return $ ofEither (\a -> (l' <= a) && (a <= h')) outcome

    describe "mapPoints"
        . it "mapping with identity doesn't change the map"
        . property
        $ \(SimpleGraph graph) -> mapPoints id graph === graph

    describe "mapPoints" . it "condenses duplicates correctly" . property $ do
        (SimpleGraph graph) <- arbitrary
        dupPoint            <- Unsafe.head <$> shuffle (assocs graph)
        let outcome  = mapPoints (const dupPoint) graph
        let expected = mkGraph [dupPoint]
        return $ outcome `shouldBe` expected

    describe "mkGraph"
        . it "condenses duplicates correctly (last one wins)"
        . property
        $ \(a :: [(Int, Int)]) -> do
              (Positive i) <- arbitrary
              let duplicates = concat . transpose $ replicate i a
              return $ mkGraph a `shouldBe` mkGraph duplicates

    describe "mkGraph"
        . it "does not remove elements that duplicate on the Y axis"
        . property
        $ \(y :: Int) -> do
              (Unique (xs :: [Int])) <- arbitrary
              let points = (, y) <$> sort xs
              return $ assocs (mkGraph points) `shouldBe` points

    describe "member"
        . it "correctly identifies members of a graph"
        . property
        $ \(Unique xs) -> do
              ys <- vectorOf (length xs) arbitrary
              let points = xs `zip` ys
              let graph  = mkGraph points :: Graph Int Int
              return $ all (`member` graph) points

    describe "member"
        . it "correctly idenitifies non-members of a graph"
        . property
        $ \(Unique xs) -> do
              ys <- vectorOf (length xs) arbitrary
              let graph = mkGraph (xs `zip` ys) :: Graph Int Int
              return
                  $ forAll (arbitrary `suchThat` (`notElem` (assocs graph)))
                  $ not
                  . (`member` graph)
