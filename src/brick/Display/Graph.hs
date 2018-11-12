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
  , mapPoints
  , mapPointsM
  , mkGraph
  , boundsX
  , boundsY
  , member
  )
where

import           Normalisation
import           Data.Hourglass
import           Control.Lens            hiding ( from
                                                , to
                                                )
import           Data.List                      ( sortBy
                                                , sort
                                                )
import           Data.Function                  ( on )
import qualified Data.Map                      as M
import           Control.Monad.Log
import           Data.Text.Prettyprint.Doc
import           Graphite

data Graph x y = NoData | Graph (M.Map x y) deriving (Show, Eq)

class Graphable n x y where
  extract :: n -> (x, y)

class (Ord n, Ord n') => Scaled n n' where
  scale :: n -> (n, n) -> (n', n') -> Either NormalisationFailure n'

instance (Integral n) => Scaled Double n where
  scale v from to = round <$> normalise from to' v
    where to' = over each fromIntegral to

instance (Integral n) => Scaled Seconds n where
  scale v from to = round <$> normalise from' to' (toRational v)
    where
      get (Seconds i) = i
      from' = over each (toRational . get) from
      to' = over each toRational to

instance Graphable DataPoint Seconds Double where
  extract DataPoint {value=v, time=(Elapsed t)} = (t, v)

boundsX :: (Num x, Ord x) => Graph x y -> (x, x)
boundsX NoData        = (0, 0)
boundsX (Graph _data) = getBounds $ M.keys _data
  where getBounds ns = (head ns, last ns)

boundsY :: (Num y, Ord y) => Graph x y -> (y, y)
boundsY NoData        = (0, 0)
boundsY (Graph _data) = getBounds . sort . M.elems $ _data
  where getBounds ns = (head ns, last ns)

mkGraph :: (Ord x, Ord y) => [(x, y)] -> Graph x y
mkGraph [] = NoData
mkGraph ps = Graph . M.fromList . sortBy (compare `on` fst) $ ps

assocs :: Graph x y -> [(x, y)]
assocs NoData        = []
assocs (Graph _data) = M.toList _data

member :: (Ord x, Ord y) => (x, y) -> Graph x y -> Bool
member _      NoData        = False
member (x, y) (Graph _data) = M.member x _data && (_data M.! x) == y

mapPoints
  :: (Ord x', Ord y') => ((x, y) -> (x', y')) -> Graph x y -> Graph x' y'
mapPoints _ NoData        = NoData
mapPoints f (Graph _data) = mkGraph $ f <$> M.toList _data

mapPointsM
  :: (MonadLog (Doc String) m, Show x, Show y, Show x', Show y', Ord x', Ord y')
  => ((x, y) -> m (x', y'))
  -> Graph x y
  -> m (Graph x' y')
mapPointsM _ NoData        = pure NoData
mapPointsM f (Graph _data) = do
  k <- f `mapM` M.toList _data

  logMessage . pretty $ "The transformed list: " ++ show k

  let a = mkGraph k

  logMessage . pretty $ "The the mkGraph list: " ++ show a

  return a

toMap :: (Ord x, Ord y) => Graph x y -> M.Map x y
toMap NoData        = mempty
toMap (Graph _data) = _data

size :: Graph x y -> Int
size NoData        = 0
size (Graph _data) = M.size _data
