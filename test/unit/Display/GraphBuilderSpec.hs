module Display.GraphBuilderSpec where

import           Test.Hspec                    as HS


spec :: HS.Spec
spec =
    describe "GraphBuilder"
        $ it "presents the string \"No Data\" when no data is available"
        $ pendingWith "being left until mock terminal usage becomes clearer"

