{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Orphans where

import           Test.QuickCheck.GenT
import           Control.Monad.Log

deriving instance MonadGen m => MonadGen ( DiscardLoggingT msg m )

instance MonadGen m => MonadGen ( ExceptT e m ) where
    liftGen = lift . liftGen
    variant n = ExceptT . variant n . runExceptT
    sized gen = ExceptT . sized $ runExceptT . gen
    resize n = ExceptT . resize n . runExceptT
    choose = ExceptT . fmap Right . choose

instance MonadGen m => MonadGen ( ReaderT r m ) where
    liftGen = lift . liftGen
    variant n op = ReaderT $ variant n . runReaderT op
    sized gen = ReaderT $ \r -> sized $ usingReaderT r . gen
    resize n op = ReaderT $ resize n . runReaderT op
    choose = ReaderT . const . choose
