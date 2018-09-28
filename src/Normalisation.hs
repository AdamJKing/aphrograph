
module Normalisation where

normaliseRange :: (Show n, Fractional n, Ord n) => [n] -> (n, n) -> [n]
normaliseRange ns (smallest, largest) =
  normalise (minimum ns, maximum ns) (smallest, largest) <$> ns

normaliseErr :: (Show a) => a -> (a, a) -> String
normaliseErr i a =
  "normalised number must be in range. [" ++ show i ++ " was not in " ++ show a

-- (i - min(a)) * (max(b) - min(b))
-- --------------------------------  + min(b)
--      (max(a) - min(a))
normalise :: (Ord a, Show a, Fractional a) => (a, a) -> (a, a) -> a -> a
normalise a b i
  | (max' a < i) || (min' a > i) = error $ normaliseErr i a
  | max' a == min' a             = max' a
  | a == b                       = i
  | otherwise = (i - min' a) * ((max' b - min' b) / (max' a - min' a)) + min' b
 where
  max' (x, y) = max x y
  min' (x, y) = min x y