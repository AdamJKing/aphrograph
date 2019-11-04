{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

module App.Logging where

import           Control.Monad.Log

type Logger msg m = (IsString msg, MonadLog msg m)
