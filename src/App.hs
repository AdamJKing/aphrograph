module App where

import           Display.Graph
import           Data.Hourglass

data AppState = AppState {
    appData :: Graph Elapsed Double,
    ui_appData :: Graph Int Int
    -- ui_labels :: [String]
}

emptyState :: AppState
emptyState = AppState NoData NoData
