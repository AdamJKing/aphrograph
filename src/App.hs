{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
module App where

import           Display.Graph
import           Graphite.Types
import           App.Args                      as App
import           Control.Monad.Log


newtype AppState = AppState {
    graphData :: Graph Time Value
} deriving (Show, Eq)

emptyState :: AppState
emptyState = AppState NoData

data AppComponent = GraphView deriving (Eq, Ord, Show)

newtype AppT m a = AppT  (  (ReaderT App.Args (LoggingT Text m)) a )
  deriving (Functor, Applicative, Monad, MonadLog Text, MonadReader App.Args, MonadIO)

instance MonadTrans AppT where
  lift op = AppT (lift (lift op))

runAppT :: Handler m Text -> App.Args -> AppT m a -> m a
runAppT logger args (AppT op) = runLoggingT (runReaderT op args) logger
