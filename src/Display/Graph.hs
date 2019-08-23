{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Display.Graph
  ( Graph
  , Graphable(..)
  , size
  , toMap
  , assocs
  , mapX
  , mapPoints
  , mapPointsM
  , mkGraph
  , boundsX
  , boundsY
  , member
  , verticalAxis
  , horizontalAxis
  , extractGraph
  , Display.Graph.null
  )
where

import           Data.Function                  ( on )
import qualified Data.Map                      as M
import qualified Relude.Unsafe                 as Unsafe
import           Graphite.Types


newtype Graph x y = Graph { _data :: M.Map x y }
  deriving (Show, Eq, Semigroup, Monoid)

class (Ord x, Ord y) => Graphable n x y where
  extract :: n -> (x, y)

instance Graphable DataPoint Time Value where
  extract DataPoint { value = v, time = t } = (t, v)

boundsX :: (Ord x) => Graph x y -> (x, x)
boundsX (Graph g) = if M.null g
  then error "boundsX on empty graph"
  else getBounds (M.keys g)
  where getBounds ns = (Unsafe.head ns, Unsafe.last ns)

boundsY :: (Num y, Ord y) => Graph x y -> (y, y)
boundsY (Graph g) = if M.null g
  then error "boundsY on empty graph"
  else getBounds (sort (M.elems g))
  where getBounds ns = (Unsafe.head ns, Unsafe.last ns)

mkGraph :: (Ord x, Ord y) => [(x, y)] -> Graph x y
mkGraph = Graph . M.fromList . sortBy (compare `on` fst)

extractGraph :: Graphable a x y => [a] -> Graph x y
extractGraph = mkGraph . fmap extract

assocs :: Graph x y -> [(x, y)]
assocs = M.toList . _data

member :: (Ord x, Ord y) => (x, y) -> Graph x y -> Bool
member (x, y) (Graph _data) = M.member x _data && (_data M.! x) == y

mapX :: (Ord x', Ord y) => (x -> x') -> Graph x y -> Graph x' y
mapX f = mapPoints (\(x, y) -> (f x, y))

null :: Graph x y -> Bool
null = M.null . _data

mapPoints
  :: (Ord x', Ord y') => ((x, y) -> (x', y')) -> Graph x y -> Graph x' y'
mapPoints f g = mkGraph $ f <$> M.toList (_data g)

mapPointsM
  :: (Monad m, Ord x', Ord y')
  => ((x, y) -> m (x', y'))
  -> Graph x y
  -> m (Graph x' y')
mapPointsM f g = mkGraph <$> mapM f (M.toList (_data g))

toMap :: (Ord x, Ord y) => Graph x y -> M.Map x y
toMap = _data

size :: Graph x y -> Int
size = M.size . _data

verticalAxis :: Graph x y -> [y]
verticalAxis = M.elems . _data

horizontalAxis :: Graph x y -> [x]
horizontalAxis = M.keys . _data
