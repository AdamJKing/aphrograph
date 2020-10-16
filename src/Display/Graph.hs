{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Display.Graph
  ( Graph,
    Graphable (..),
    toMap,
    assocs,
    mkGraph,
    boundsX,
    boundsY,
    verticalAxis,
    horizontalAxis,
    Display.Graph.null,
  )
where

import qualified Data.Map as M
import qualified Data.Set as Set
import Graphite.Types ( DataPoint(..), Time, Value )

newtype Graph x y = Graph {_data :: M.Map x (Set y)}
  deriving (Show, Eq, Semigroup, Monoid)

class (Ord x, Ord y) => Graphable n x y where
  extract :: n -> (x, y)

instance Graphable DataPoint Time Value where
  extract DataPoint {value = v, time = t} = (t, v)

boundsX :: Ord x => Graph x y -> (x, x)
boundsX (Graph g) =
  case nonEmpty (sort (M.keys g)) of
    Just ns -> (head ns, last ns)
    Nothing -> error "boundsX on empty graph"

boundsY :: (Num y, Ord y) => Graph x y -> (y, y)
boundsY (Graph g) =
  case nonEmpty (Set.toAscList $ Set.unions $ M.elems g) of
    Just ns -> (head ns, last ns)
    Nothing -> error "boundsY on empty graph"

mkGraph :: (Ord x, Ord y, Foldable t) => t (x, y) -> Graph x y
mkGraph points = Graph $ foldr collectToSet mempty points
  where
    collectToSet (x, y) = M.insertWith Set.union x (one y)

assocs :: Graph x y -> [(x, y)]
assocs (Graph g) = do
  (x, ys) <- M.toList g
  (x,) <$> Set.toList ys

null :: Graph x y -> Bool
null = M.null . _data

toMap :: (Ord x, Ord y) => (Set y -> y) -> Graph x y -> M.Map x y
toMap combineBucket g = combineBucket <$> _data g

verticalAxis :: Graph x y -> [y]
verticalAxis g = snd <$> assocs g

horizontalAxis :: Graph x y -> [x]
horizontalAxis g = fst <$> assocs g
