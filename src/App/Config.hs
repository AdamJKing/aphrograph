{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module App.Config where

import           Graphite.Types
import           Control.Lens.TH

data GraphiteConfig = GraphiteConfig {
      fromTime :: From
    , toTime :: Maybe To
    , targetArg :: Text
    , graphiteUrl :: GraphiteUrl }
    deriving ( Show, Generic )

makeLenses ''GraphiteConfig

newtype Config = Config GraphiteConfig
    deriving ( Show, Generic )
