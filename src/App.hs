{-# LANGUAGE TemplateHaskell #-}

module App where

import           Display.Graph
import           Graphite
import           Control.Lens

data UiElements = UI {
    _displayData :: Graph Int Int,
    _displayLabels :: [Text]
}

data AppState = AppState {
    _appData :: Graph Time Value,
    _ui :: UiElements
}

emptyState :: AppState
emptyState = AppState NoData emptyUi

emptyUi :: UiElements
emptyUi = UI NoData mempty

makeLenses ''AppState
makeLenses ''UiElements
