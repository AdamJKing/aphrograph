{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module App.StateSpec
  ( spec,
  )
where

import ArbitraryInstances ()
import Test.Hspec (Spec, it, pending)

spec :: Spec
spec = it "no tests" pending