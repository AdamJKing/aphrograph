{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module App
  ( App
  , AppLike
  , AppError(..)
  , AppState(..)
  , ActiveState(..)
  , AppLogger
  , runAppM
  , appArgs
  , metricsView
  , graphData
  , timezone
  , constructDefaultContext
  )
where

import           Fmt
import           App.Args                      as App
import           Control.Lens
import           Control.Monad.Except    hiding ( runExceptT )
import           Data.Time.LocalTime
import           Display.Graph
import           Network.HTTP.Req              as Req
import           Graphite.Types
import           Graphite
import qualified Text.Show                     as TS
import           Control.Lens.Getter            ( view )
import           Control.Monad.Log             as Log
import           Text.Show.Functions            ( )

data AppError = forall e. Exception e => AppError e

instance Exception AppError where
  displayException (AppError err) = displayException err

instance TS.Show AppError where
  show = displayException

instance Eq AppError where
  _ == _ = False

newtype AppM s m a = AppM {
  _runApp :: ReaderT s (ExceptT AppError m) a
} deriving (
    Functor
  , Applicative
  , Monad
  , MonadIO
  , MonadError AppError
  , MonadReader s
  )

type App = AppM ActiveState IO

type AppLogger = MonadLog Fmt.Builder

type AppLike m
  = (MonadReader ActiveState m, MonadGraphite m, MonadLog Fmt.Builder m)

data ActiveState = ActiveState {
     _metricsView :: Maybe [Metric]
   , _graphData :: Graph Time Value
   , _timezone ::  TimeZone
   , _appArgs :: App.Args
   , _logger :: Log.Handler App Text
} deriving ( Show , Generic )
  deriving Eq via (Ignoring (Log.Handler App Text) ActiveState)

makeLenses ''ActiveState

runAppM :: ActiveState -> App a -> IO a
runAppM activeState = unsafeHandleError . usingReaderT activeState . _runApp
 where
  unsafeHandleError err = runExceptT err <&> \case
    Left  appError -> error (fromString $ displayException appError)
    Right result   -> result

constructDefaultContext :: Log.Handler IO Text -> App.Args -> IO AppState
constructDefaultContext handler args = do
  timezone' <- liftIO getCurrentTimeZone
  return . Active $ ActiveState { _metricsView = Nothing
                                , _graphData   = mempty
                                , _timezone    = timezone'
                                , _appArgs     = args
                                , _logger      = liftIO . handler
                                }

data AppState = Active ActiveState | Failed AppError
  deriving ( Generic, Show, Eq )

instance MonadLog Fmt.Builder App where
  logMessageFree addLog =
    let logs = addLog (\msg -> [fmt msg])
    in  do
          logf <- view logger
          forM_ logs logf

instance MonadHttp App where
  handleHttpException = throwError . AppError . \case
    (JsonHttpException    reason) -> ParsingError (fromString reason)
    (VanillaHttpException reason) -> HttpError reason

instance MonadGraphite App where
  listMetrics = view (appArgs . graphiteUrl) >>= listMetricsHttp

  getMetrics request = do
    url <- view (appArgs . graphiteUrl)
    getMetricsHttp url request
