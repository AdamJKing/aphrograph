{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Display.WidgetsSpec where

import           Test.Hspec                    as HS
import           ArbitraryInstances             ( )
import           Display.Projection

instance (Monad m) => MonadProjector a a (IdentityT m) where
  project = pure

spec :: HS.Spec
spec = describe "Widgets" $ it "has no purpose yet" $ True `shouldBe` True
