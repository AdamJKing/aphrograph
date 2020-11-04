{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module EventsSpec
  ( spec,
  )
where

import Test.Hspec (Spec, describe, pendingWith)
import Test.Hspec.QuickCheck (prop)

spec :: Spec
spec = do
  prop "pressing (m) opens the browser when closed" $ pendingWith "waiting for keypress tests"
  prop "pressing (m) closes the browser when open" $ pendingWith "waiting for keypress tests"

  describe "pressing Enter in metrics view" $ do
    prop "selects the metric under the cursor as the current metric when metrics are available" $ pendingWith "waiting for keypress tests"
    prop "when there are no metrics available, the result is the no-data-display" $ pendingWith "waiting for keypress tests"
