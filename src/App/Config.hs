{-# LANGUAGE TemplateHaskell #-}

module App.Config where

import           Graphite.Types
import           Control.Lens.TH

data GraphiteConfig = GraphiteConfig {
      fromTime :: From
    , toTime :: Maybe To
    , targetArg :: Text
    , graphiteUrl :: GraphiteUrl }

makeLenses ''GraphiteConfig

newtype AppConfig = AppConfig GraphiteConfig
