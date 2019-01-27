{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module LabelsSpec where

import           Test.Hspec                    as HS
import           Labels
import           CommonProperties
import           Display.Graph
import           Normalisation
import           Control.Lens            hiding ( from
                                                , to
                                                )


instance (Real r) => Scaled r Int where
  scale v from to = round <$> normalise from' to' (toRational v)
   where
    from' = over each toRational from
    to'   = over each toRational to

instance (Real r) => Scaled r Double where
  scale v from to = fromRational <$> normalise from' to' (toRational v)
   where
    from' = over each toRational from
    to'   = over each toRational to

spec :: HS.Spec
spec = describe "Labels" . describe "labels" $ do
  it "generates labels for clean ints"
    $ let proj :: Projection Int Int
          proj = Projection (0, 10) (0, 100)
      in  ofEither (`shouldBe` [ Label (show i) (i * 10) | i <- [0 .. 10] ])
                   (runReader (runExceptT labels) proj)

  it "generates labels for non-integers"
    $ let proj :: Projection Double Int
          proj = Projection (0, 1) (0, 100)
      in  ofEither
            (`shouldBe` [ Label "0.0" 0
                        , Label "0.1" 10
                        , Label "0.2" 20
                        , Label "0.3" 30
                        , Label "0.4" 40
                        , Label "0.5" 50
                        , Label "0.6" 60
                        , Label "0.7" 70
                        , Label "0.8" 80
                        , Label "0.9" 90
                        , Label "1.0" 100
                        ]
            )
            (runReader (runExceptT labels) proj)
