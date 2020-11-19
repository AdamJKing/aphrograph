{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Display.GraphSpec
  ( spec,
  )
where

import ArbitraryInstances ()
import CommonProperties (range)
import Data.List.NonEmpty ((<|))
import qualified Data.List.NonEmpty as NE
import Display.Graph as Graph
  ( Graphable (extract),
    assocs,
    boundsX,
    boundsY,
    mkGraph,
  )
import Graphite.Types (DataPoint (time, value))
import Test.Hspec as HS (Spec, describe, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    Gen,
    Positive (Positive),
    choose,
    counterexample,
    forAll,
    getInfiniteList,
    shuffle,
    sized,
    vectorOf,
    (===),
  )

nonEmptyListOf :: Gen a -> Gen (NonEmpty a)
nonEmptyListOf gen = sized $ \i ->
  do
    n <- gen
    ns <- vectorOf i gen
    return (n :| ns)

spec :: HS.Spec
spec = describe "Graph" $ do
  describe "Graphable (DataPoint)"
    . describe "extract"
    . prop "extracts graphable data from a type"
    $ \dp ->
      extract dp === (time dp, value dp)
  describe "boundsX"
    . prop "correctly gets the X bounds (Integers)"
    $ do
      (lower, upper) <- range @Integer
      xs' <- nonEmptyListOf (choose (lower, upper))
      let xs = toList . NE.sort . NE.nub $ (lower <| upper <| xs')
      ys <- getInfiniteList @Integer <$> arbitrary
      graph <- Graph.mkGraph <$> shuffle (xs `zip` ys)
      return $ counterexample (show xs) (boundsX graph `shouldBe` (lower, upper))
  describe "boundsY"
    . prop "correctly gets the Y bounds (Integers)"
    $ do
      (lower, upper) <- range @Integer
      ys' <- nonEmptyListOf (choose (lower, upper))
      let ys = toList . NE.sort . NE.nub $ (lower <| upper <| ys')
      xs <- getInfiniteList @Integer <$> arbitrary
      graph <- Graph.mkGraph <$> shuffle (xs `zip` ys)
      return (boundsY graph `shouldBe` (lower, upper))
  describe "mkGraph"
    . prop "condenses duplicates correctly (last one wins)"
    $ \(a :: [(Int, Int)]) -> do
      (Positive i) <- arbitrary
      let duplicates = concat . transpose $ replicate i a
      return $ mkGraph a `shouldBe` mkGraph duplicates
  describe "mkGraph"
    . prop "does not remove elements that duplicate on the Y axis"
    $ \(y :: Int) -> do
      xs <- arbitrary @(Set Int)
      let points = (,y) <$> sort (toList xs)
      return $ assocs (mkGraph points) `shouldBe` points
  prop "order of input datapoints doesn't affect the equality of the graph" $
    \(xs :: [(Int, Int)]) -> forAll (shuffle xs) $ \shuffled -> mkGraph xs === mkGraph shuffled
