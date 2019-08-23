{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Prelude
  ( module Relude
  , minMax
  , Ignoring(..)
  )
where

import           Relude
import           GHC.Generics
import           Data.Singletons
import           Data.Singletons.Prelude        ( Sing(SFalse, STrue) )
import           Data.Singletons.Prelude.Eq

minMax :: (Ord a) => NonEmpty a -> (a, a)
minMax (x :| []) = (x, x)
minMax (x :| xs) = (min mn x, max mx x) where (mn, mx) = minMax $ fromList xs

newtype Ignoring :: Type -> Type -> Type where
  Ignoring ::a -> Ignoring ignored a

class GEq ( ignored :: Type ) f where
  geq :: f a -> f a -> Bool

instance GEq ignored a => GEq ignored (M1 i c a) where
  geq (M1 x) (M1 y) = geq @ignored x y

instance ( GEq ignored a , GEq ignored b) => GEq ignored (a :*: b) where
  geq (a1 :*: b1) (a2 :*: b2) = geq @ignored a1 a2 && geq @ignored b1 b2

type family Unless (b :: Bool) (c :: Constraint) :: Constraint where
  Unless 'True _ = ()
  Unless 'False b = b

instance (Unless (DefaultEq ignored a) ( Eq a ), SingI (DefaultEq ignored a)) => GEq ignored (K1 i a) where
  geq (K1 x) (K1 y) = case sing @(DefaultEq ignored a) of
    SFalse -> x == y
    STrue  -> True

instance (Generic a, GEq ignored (Rep a)) => Eq (Ignoring ignored a) where
  Ignoring x == Ignoring y = geq @ignored (from x) (from y)
