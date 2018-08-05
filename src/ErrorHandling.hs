module ErrorHandling where

import           Control.Exception.Base         ( throwIO )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad                  ( unless )
import           UI.NCurses                     ( Curses
                                                , throwCurses
                                                , catchCurses
                                                )
import           Control.Monad.Catch            ( ExitCase(..)
                                                , fromException
                                                , toException
                                                , MonadThrow(..)
                                                , MonadCatch(..)
                                                , MonadMask(..)
                                                )


instance MonadThrow Curses where
    throwM e = case fromException $ toException e of
        Just cursesEx -> throwCurses cursesEx
        Nothing -> liftIO $ throwIO e

instance MonadCatch Curses where
    catch dangerous errorHandler = catchCurses dangerous $
     \e -> case fromException $ toException e of
        Just cursesEx -> errorHandler cursesEx
        Nothing -> throwM $ toException e

instance MonadMask Curses where
    mask restore = restore id
    uninterruptibleMask restore = restore id
    generalBracket acquire release use = mask $ \unmasked -> do
        resource <- acquire
        b <- unmasked (use resource) `catch` \e -> do
          _ <- release resource (ExitCaseException e)
          throwM e
        c <- release resource (ExitCaseSuccess b)
        return (b, c)

