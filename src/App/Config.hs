{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Config where

import Control.Lens.TH (makeLenses)
import Data.Time (TimeZone)
import Graphite.Types (From, GraphiteUrl, To)

data GraphiteConfig = GraphiteConfig
  { _fromTime :: From,
    _toTime :: Maybe To,
    _targetArg :: Text,
    _graphiteUrl :: GraphiteUrl
  }
  deriving (Show, Generic)

makeLenses ''GraphiteConfig

data Config = Config
  { _graphiteConfig :: GraphiteConfig,
    _timezone :: TimeZone
  }
  deriving (Show, Generic)

makeLenses ''Config
