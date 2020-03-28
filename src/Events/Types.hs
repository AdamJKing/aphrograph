module Events.Types where

import App.Components
import qualified Brick.Types as Brick

data AppEvent = UpdateEvent
  deriving (Show, Eq)

type SystemEvent = Brick.BrickEvent AppComponent AppEvent
