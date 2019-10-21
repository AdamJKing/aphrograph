{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module App where

import qualified App.Config                    as App
import qualified App.State                     as App
import qualified Network.HTTP.Req              as Req
import           Data.HList.ContainsType
import           Fmt
import           Control.Monad.Trans.MultiReader
import           Control.Monad.Log             as Log
import           Text.Show.Functions            ( )
import           Data.HList.HList
import           Control.Monad.Except           ( MonadError(throwError) )
import qualified Brick.Types                   as Brick
import           Display.Widgets                ( AppComponent )

type Logger = Log.Handler AppM [Text]

type AppDependencies = '[App.Config, Logger]

newtype AppM a = AppM { _unApp :: ExceptT App.Error ( MultiReaderT AppDependencies (Brick.EventM AppComponent) ) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError App.Error)

liftEventM :: Brick.EventM AppComponent a -> AppM a
liftEventM = AppM . lift . lift

runApp :: Logger -> App.Config -> AppM a -> Brick.EventM AppComponent a
runApp logger conf app = do
  let deps = conf :+: logger :+: HNil
  runMultiReaderTNil (withMultiReaders deps (runExceptT (_unApp app))) <&> \case
    Right ret -> ret
    Left  err -> error ("Unhandled app error: " <> toText (displayException err))

instance {-# OVERLAPPING #-} (ContainsType a AppDependencies) => MonadMultiReader a AppM where
  mAsk = AppM mAsk

instance MonadLog Fmt.Builder AppM  where
  logMessageFree addLog = do
    appLogger <- mAsk @Logger
    appLogger $ addLog $ \logLine -> one (fmt logLine)

instance Req.MonadHttp AppM where
  handleHttpException = throwError . App.HttpError
