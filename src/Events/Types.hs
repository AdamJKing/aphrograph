module Events.Types where

import App.Components
import qualified Brick.Types as Brick
import Display.Graph
import Graphite.Types

data AppEvent = TriggerUpdate | GraphUpdate (Graph Time Value)
  deriving (Show, Eq)

type SystemEvent = Brick.BrickEvent AppComponent AppEvent

class Monad m => GraphViewer m where
  updateGraph :: m (Graph Time Value)
