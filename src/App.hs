{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module App
  ( AppState(..)
  , AppComponent(..)
  , emptyState
  , AppError(..)
  , App
  , runApp
  , graphData
  , timezone
  )
where

import           Display.Graph
import           Graphite.Types
import           App.Args                      as App
import           Control.Monad.Log
import           Control.Lens.TH
import           Data.Time.LocalTime


data AppState = AppState {
    _graphData :: Graph Time Value,
    _timezone :: TimeZone
} deriving (Show, Eq)

makeLenses ''AppState

emptyState :: IO AppState
emptyState = AppState NoData <$> getCurrentTimeZone

data AppComponent = GraphView deriving (Eq, Ord, Show)

data AppError where
  AppError ::e -> AppError

newtype App a = App  {
  _unApp :: (ExceptT AppError (ReaderT App.Args (LoggingT Text IO))) a
}
  deriving (Functor, Applicative, Monad, MonadLog Text, MonadReader App.Args, MonadIO)

instance MonadFail App where
  fail = liftIO . fail

runApp :: Handler IO Text -> App.Args -> App a -> IO (Either AppError a)
runApp logger args =
  (`runLoggingT` logger) . usingReaderT args . runExceptT . _unApp
