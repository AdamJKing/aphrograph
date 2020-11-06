{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module EventsSpec
  ( spec,
  )
where

import Test.Hspec (Spec, describe, it, pendingWith)

spec :: Spec
spec = do
  it "pressing (m) opens the browser when closed" $ pendingWith "waiting for keypress tests"
  it "pressing (m) closes the browser when open" $ pendingWith "waiting for keypress tests"

  describe "pressing Enter in metrics view" $ do
    it "selects the metric under the cursor as the current metric when metrics are available" $ pendingWith "waiting for keypress tests"
    it "when there are no metrics available, the result is the no-data-display" $ pendingWith "waiting for keypress tests"
