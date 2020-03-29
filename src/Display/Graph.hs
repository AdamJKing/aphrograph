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
import Graphite.Types
import qualified Relude.Unsafe as Unsafe

newtype Graph x y = Graph {_data :: M.Map x (Set y)}
  deriving (Show, Eq, Semigroup, Monoid)

class (Ord x, Ord y) => Graphable n x y where
  extract :: n -> (x, y)

instance Graphable DataPoint Time Value where
  extract DataPoint {value = v, time = t} = (t, v)

boundsX :: (Ord x) => Graph x y -> (x, x)
boundsX (Graph g) = if M.null g then error "boundsX on empty graph" else getBounds (sort (M.keys g))
  where
    getBounds ns = (Unsafe.head ns, Unsafe.last ns)

boundsY :: (Num y, Ord y) => Graph x y -> (y, y)
boundsY (Graph g) =
  if M.null g
    then error "boundsY on empty graph"
    else getBounds (Set.toAscList $ Set.unions $ M.elems g)
  where
    getBounds ns = (Unsafe.head ns, Unsafe.last ns)

mkGraph :: (Ord x, Ord y) => [(x, y)] -> Graph x y
mkGraph = Graph . M.fromAscListWith Set.union . sortWith fst . (one <<$>>)

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
