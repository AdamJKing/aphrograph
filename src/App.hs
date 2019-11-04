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
import           Control.Lens.Zoom
import           Control.Lens.Internal.Zoom     ( Effect(..) )

type Logger m = Log.Handler m Text

newtype AppT r m a = MkAppT { _unApp :: ReaderT r (  ExceptT App.Error (LoggingT Fmt.Builder m ) ) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadLog Fmt.Builder, MonadReader r, MonadError App.Error)

instance MonadTrans ( AppT r ) where
  lift = MkAppT . lift . lift . lift

runApp :: Monad m => Logger m -> App.Config -> AppT App.Config m a -> m a
runApp logger conf action = convertToRuntimeError
  <$> runLoggingT (runExceptT (usingReaderT conf (_unApp action))) (logger . fmt)
  where convertToRuntimeError = either (error . ("Unhandled app error: " <>) . toText . displayException) id

instance MonadIO m => Req.MonadHttp (AppT r m) where
  handleHttpException = throwError . App.HttpError

type instance Magnified (AppT r m) = Effect (ExceptT App.Error (LoggingT Fmt.Builder m))

instance (Monad m, MonadReader a (AppT a m), MonadReader b (AppT b m)) => Magnify (AppT b m) (AppT a m) b a where
  magnify lens (MkAppT op) = MkAppT $ magnify lens op

handleEvent'
  :: Brick.BrickEvent n AppEvent -> App.ActiveState -> AppT App.Config (Brick.EventM n) (Brick.Next App.ActiveState)
handleEvent' event =
  magnify App.graphiteConfig
    . handleEvent
        (EventHandler { continue = lift . Brick.continue, ignore = lift . Brick.continue, stop = lift . Brick.halt })
        event
