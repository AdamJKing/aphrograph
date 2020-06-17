module EventsSpec
  ( spec,
  )
where

import App.State as App
import ArbitraryInstances ()
import CommonProperties
import Control.Lens.Operators
import Test.Hspec
  ( Spec,
    describe,
  )
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Monadic

spec :: Spec
spec = describe "Events" $ pending
