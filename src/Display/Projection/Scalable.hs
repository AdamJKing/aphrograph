{-# LANGUAGE FlexibleInstances #-}

module Display.Projection.Scalable where

import Control.Lens
  ( each,
    over,
  )
import Data.Decimal
import Data.Time.Clock
import Normalisation

class Real b => Scalable b where
  scale :: Real a => a -> (a, a) -> (b, b) -> b

scale2Integral :: (Integral b, Real a) => a -> (a, a) -> (b, b) -> b
scale2Integral v base target = round $ normalise rationalBase rationalTarget (toRational v)
  where
    rationalBase = over each toRational base
    rationalTarget = over each toRational target

scale2Fractional :: (Real a, Real b, Fractional b) => a -> (a, a) -> (b, b) -> b
scale2Fractional v base target = fromRational $ normalise rationalBase rationalTarget (toRational v)
  where
    rationalBase = over each toRational base
    rationalTarget = over each toRational target

instance Scalable Int where
  scale = scale2Integral
  {-# INLINEABLE scale #-}

instance Scalable Integer where
  scale = scale2Integral
  {-# INLINEABLE scale #-}

instance Integral n => Scalable (Ratio n) where
  scale = scale2Fractional
  {-# INLINEABLE scale #-}

instance Scalable Int64 where
  scale = scale2Integral
  {-# INLINEABLE scale #-}

instance Scalable Double where
  scale = scale2Fractional
  {-# INLINEABLE scale #-}

instance Scalable Decimal where
  scale = scale2Fractional
  {-# INLINEABLE scale #-}

instance Scalable NominalDiffTime where
  scale = scale2Fractional
  {-# INLINEABLE scale #-}
