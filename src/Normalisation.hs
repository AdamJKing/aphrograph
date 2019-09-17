module Normalisation
  ( normalise
  )
where


diff :: Num a => (a, a) -> a
diff (a, b) = b - a

normalise :: (Ord a, Fractional a, Show a) => (a, a) -> (a, a) -> a -> a
normalise origin target value
  | origin == target = value
  | 0 == diff target = fst target
  | 0 == diff origin = error "Cannot normalise a value that has no minimum and maximum value."
  | let (l, h) = origin in (l > value || h < value) = error "Cannot normalise a value outside of the given range."
  | otherwise        = normalise' origin target value

normalise' :: (Fractional a, Num a) => (a, a) -> (a, a) -> a -> a
normalise' origin target value =
  let originWidth = diff origin
      targetWidth = diff target
  in  (((value - fst origin) / originWidth) * targetWidth) + fst target
