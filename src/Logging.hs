{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Logging where

import           System.IO
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Control.Monad.Log
import           Control.Monad                  ( unless )
import           UI.NCurses
import           Graph
import           Graphite
import           ErrorHandling
import           Data.Text.Prettyprint.Doc
import           Control.Monad.Catch
import           Control.Exception.Base         ( throwIO )
import           Control.Monad.Trans.Class      ( lift )
import           Time.Types                     ( toSeconds
                                                , Hours(..)
                                                )
import           Streaming
import qualified Streaming.Prelude             as S

newtype LoggingCursesStream a r = LoggingCursesStream (Stream (Of a) (LoggingT (Doc String) Curses) r)
    deriving ( Applicative
             , Functor
             , Monad
             , MonadIO
             )

instance (Functor f, MonadLog message m) => MonadLog message (Stream f m) where
  logMessageFree foldMap = lift $ logMessageFree foldMap

newtype LoggingCurses a = LoggingCurses (LoggingT (Doc String) Curses a)
    deriving ( Applicative
             , Functor
             , Monad
             , MonadIO
             , MonadLog (Doc String)
             )

runLoggingCurses :: LoggingCurses a -> IO a
runLoggingCurses (LoggingCurses lc) =
  withFile "aphrograph.log" WriteMode $ \logfile ->
    runCurses
      . withFDHandler defaultBatchingOptions logfile 0.4 80
      $ \logger -> runLoggingT lc logger

liftCurses :: Curses a -> LoggingCurses a
liftCurses = LoggingCurses <$> lift
