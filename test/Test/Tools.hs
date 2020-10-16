{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Test.Tools where

import Graphite.Types (MonadGraphite (..))

newtype Lifted t m a = Lifted (t m a)
  deriving (Functor, Applicative, Monad)

instance (MonadGraphite m, Monad (t m), MonadTrans t) => MonadGraphite (Lifted t m) where
  listMetrics = Lifted (lift listMetrics)
  getMetrics req = Lifted (lift (getMetrics req))
