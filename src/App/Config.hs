{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Config where

import Control.Lens.TH
import Graphite.Types

data GraphiteConfig
  = GraphiteConfig
      { _fromTime :: From,
        _toTime :: Maybe To,
        _targetArg :: Text,
        _graphiteUrl :: GraphiteUrl
      }
  deriving (Show, Generic)

makeLenses ''GraphiteConfig

newtype Config = Config {_graphiteConfig :: GraphiteConfig}
  deriving (Show, Generic)

makeLenses ''Config
