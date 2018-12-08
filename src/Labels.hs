{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Labels where

class Label a where
    label :: Int -> [a] -> [String]

newtype Discrete a = Discrete { getDiscrete :: a }
    deriving (Eq, Ord, Enum, Show)

newtype Continuous a = Continuous { getContinuous :: a }
    deriving (Eq, Ord)

instance (Integral a, Show a, Num a) => Label (Discrete a) where
    label maxWidth xs = show <$> label' 1
      where
        label' i =
            let labels      = [minimum xs .. maximum xs]
                displayable = getDiscrete <$> every i labels
            in  if totalPosibleLength displayable > maxWidth
                    then label' (i + 1)
                    else displayable

        every n (i : is) = i : every n (drop n is)
        every _ []       = []

instance (RealFrac a, Ord a, Show a) => Label (Continuous a) where
    label maxWidth xs =
        let (Continuous mn) = minimum xs
            (Continuous mx) = maximum xs
            label' x =
                let f      = (mx - mn) / fromInteger x
                    labels = [ mn + (f * fromInteger i) | i <- [0 .. x] ]
                in  if totalPosibleLength labels > fromIntegral maxWidth
                        then label' (x - 1)
                        else labels
        in  show <$> label' (toInteger $ length xs)

totalPosibleLength :: (Foldable t, Functor t, Show a) => t a -> Int
totalPosibleLength xs =
    length . foldl (\a b -> a ++ " " ++ b) "" $ fmap show xs
