{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Display.Graph
  ( Graph(NoData, Graph)
  , Graphable(..)
  , Scaled(..)
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
  )
where

import           Normalisation
import           Data.Hourglass          hiding ( Time )
import           Control.Lens            hiding ( from
                                                , to
                                                )
import           Data.Function                  ( on )
import qualified Data.Map                      as M


import           Graphite
import qualified Relude.Unsafe                 as Unsafe

data Graph x y = NoData | Graph (M.Map x y) deriving (Show, Eq)

class Graphable n x y where
  extract :: n -> (x, y)

class (Ord n, Ord n') => Scaled n n' where
  scale :: n -> (n, n) -> (n', n') -> Either NormalisationFailure n'

instance (Integral n) => Scaled Value n where
  scale v from to = round <$> normalise from to' v
    where to' = over each fromIntegral to

instance (Integral n) => Scaled Time n where
  scale v from to = round <$> normalise from' to' (toRational $ getAsInt v)
   where
    getAsInt (Time (Elapsed (Seconds i))) = i
    from' = over each (toRational . getAsInt) from
    to'   = over each toRational to

instance Graphable DataPoint Time Value where
  extract DataPoint { value = v, time = t } = (t, v)

boundsX :: (Num x, Ord x) => Graph x y -> (x, x)
boundsX NoData        = (0, 0)
boundsX (Graph _data) = getBounds $ M.keys _data
  where getBounds ns = (Unsafe.head ns, Unsafe.last ns)

boundsY :: (Num y, Ord y) => Graph x y -> (y, y)
boundsY NoData        = (0, 0)
boundsY (Graph _data) = getBounds . sort . M.elems $ _data
  where getBounds ns = (Unsafe.head ns, Unsafe.last ns)

mkGraph :: (Ord x, Ord y) => [(x, y)] -> Graph x y
mkGraph [] = NoData
mkGraph ps = Graph . M.fromList . sortBy (compare `on` fst) $ ps

assocs :: Graph x y -> [(x, y)]
assocs NoData        = []
assocs (Graph _data) = M.toList _data

member :: (Ord x, Ord y) => (x, y) -> Graph x y -> Bool
member _      NoData        = False
member (x, y) (Graph _data) = M.member x _data && (_data M.! x) == y

mapX :: (Ord x', Ord y) => (x -> x') -> Graph x y -> Graph x' y
mapX f = mapPoints (\(x, y) -> (f x, y))

mapPoints
  :: (Ord x', Ord y') => ((x, y) -> (x', y')) -> Graph x y -> Graph x' y'
mapPoints _ NoData        = NoData
mapPoints f (Graph _data) = mkGraph $ f <$> M.toList _data

mapPointsM
  :: (Monad m, Ord x', Ord y')
  => ((x, y) -> m (x', y'))
  -> Graph x y
  -> m (Graph x' y')
mapPointsM _ NoData        = pure NoData
mapPointsM f (Graph _data) = mkGraph <$> f `mapM` M.toList _data

toMap :: (Ord x, Ord y) => Graph x y -> M.Map x y
toMap NoData        = mempty
toMap (Graph _data) = _data

size :: Graph x y -> Int
size NoData        = 0
size (Graph _data) = M.size _data
