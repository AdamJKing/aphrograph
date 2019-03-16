
module Display.Widgets where

import           Brick.Types
import           Brick.Widgets.Core


cornerPiece :: Widget n
cornerPiece = padBottom Max $ padLeft Max $ txt "\9492"
