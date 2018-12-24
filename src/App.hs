module App where

import           Display.Graph
import           Graphite

data AppState = AppState {
    appData :: Graph Time Value,
    ui_appData :: Graph Int Int
    -- ui_labels :: [Text]
}

emptyState :: AppState
emptyState = AppState NoData NoData
