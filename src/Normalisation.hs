
module Normalisation where

-- deprecated
-- normaliseRange :: (Show n, Fractional n, Ord n) => [n] -> (n, n) -> [n]
-- normaliseRange ns (smallest, largest) =
--   normalise (minimum ns, maximum ns) (smallest, largest) <$> ns

rangeFrom :: (Ord n) => [n] -> (n, n)
rangeFrom xs = (minimum xs, maximum xs)

normaliseErr :: (Show a) => a -> (a, a) -> String
normaliseErr i a =
  "normalised number must be in range. [" ++ show i ++ " was not in " ++ show a

normaliseFractional
  :: (Ord a, Show a, Fractional a) => (a, a) -> (a, a) -> a -> a
normaliseFractional a b i
  | (max' a < i) || (min' a > i) = error $ normaliseErr i a
  | max' a == min' a             = max' a
  | a == b                       = i
  | otherwise = (i - min' a) * ((max' b - min' b) / (max' a - min' a)) + min' b
 where
  max' (x, y) = max x y
  min' (x, y) = min x y

normaliseIntegral :: (Ord a, Show a, Integral a) => (a, a) -> (a, a) -> a -> a
normaliseIntegral a b i
  | (max' a < i) || (min' a > i)
  = error $ normaliseErr i a
  | max' a == min' a
  = max' a
  | a == b
  = i
  | otherwise
  = (i - min' a) * ((max' b - min' b) `quot` (max' a - min' a)) + min' b
 where
  max' (x, y) = max x y
  min' (x, y) = min x y
