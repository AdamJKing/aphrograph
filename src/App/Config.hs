{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module App.Config where

import           Graphite.Types
import           Control.Lens.TH

data GraphiteConfig = GraphiteConfig {
      _fromTime :: From
    , _toTime :: Maybe To
    , _targetArg :: Text
    , _graphiteUrl :: GraphiteUrl }
    deriving ( Show, Generic )

makeLenses ''GraphiteConfig

newtype Config = Config { _graphiteConfig :: GraphiteConfig }
    deriving ( Show, Generic )

makeLenses ''Config
