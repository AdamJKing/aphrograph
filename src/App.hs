module App where

import           Graphite
import           Display.Graph

newtype AppState = AppState {
    graphData :: Graph Time Value
} deriving (Show, Eq)

emptyState :: AppState
emptyState = AppState NoData

data AppComponent = GraphView deriving (Eq, Ord, Show)
