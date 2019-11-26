{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module App where

import qualified App.Config                    as App
import qualified App.State                     as App
import qualified Network.HTTP.Req              as Req
import qualified Brick.Types                   as Brick
import qualified Brick.Main                    as Brick
import           Events
import           Fmt
import           Control.Monad.Log             as Log
import           Text.Show.Functions            ( )
import           Control.Monad.Except           ( MonadError(throwError) )
import           Control.Lens.Getter
import           Graphite.Types
import           Graphite

type Logger m = Log.Handler m Text

newtype AppT m a = MkAppT { _unApp :: ReaderT App.Config (  ExceptT App.Error (LoggingT Fmt.Builder m ) ) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadLog Fmt.Builder, MonadReader App.Config, MonadError App.Error)

instance MonadTrans AppT where
  lift = MkAppT . lift . lift . lift

runApp :: Monad m => Logger m -> App.Config -> AppT m a -> m a
runApp logger conf action = convertToRuntimeError
  <$> runLoggingT (runExceptT (usingReaderT conf (_unApp action))) (logger . fmt)
  where convertToRuntimeError = either (error . ("Unhandled app error: " <>) . toText . displayException) id

instance MonadIO m => Req.MonadHttp (AppT m) where
  handleHttpException = throwError . App.HttpError

instance MonadIO m => MonadGraphite (AppT m) where
  listMetrics = view (App.graphiteConfig . App.graphiteUrl) >>= listMetricsHttp
  getMetrics request = view (App.graphiteConfig . App.graphiteUrl) >>= getMetricsHttp ?? request

handleEvent' :: Brick.BrickEvent n AppEvent -> App.CurrentState -> AppT (Brick.EventM n) (Brick.Next App.CurrentState)
handleEvent' = handleEvent
  (EventHandler { continue = lift . Brick.continue, ignore = lift . Brick.continue, stop = lift . Brick.halt })
