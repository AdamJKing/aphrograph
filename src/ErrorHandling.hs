module ErrorHandling where

import           Debug.Trace
import           Control.Exception.Base  hiding ( catch
                                                , mask
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad                  ( unless )
import           UI.NCurses
import           Control.Monad.Catch            ( ExitCase(..)
                                                , fromException
                                                , toException
                                                , MonadThrow(..)
                                                , MonadCatch(..)
                                                , MonadMask(..)
                                                )


instance MonadThrow Curses where
    throwM = liftIO . throwM

instance MonadCatch Curses where
    catch curses handler = catchCurses curses handler'
        where handler' e = case fromException (toException e) of
                                Just e' -> handler e'
                                Nothing -> throwM e

instance MonadMask Curses where

    mask action = action restore
        where restore act = act

    uninterruptibleMask action = action restore
        where restore act = act

    generalBracket acquire release use = mask $ \unmasked -> do
        resource <- acquire
        b <- unmasked (use resource) `catch` \e -> do
          _ <- release resource (ExitCaseException e)
          throwM e
        c <- release resource (ExitCaseSuccess b)
        return (b, c)

