{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module App where

import           Data.Traversable
import           System.IO
import           Data.Foldable
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Log
import           Control.Monad.Writer
import           Control.Monad                  ( unless )
import           UI.NCurses
import           Graph
import           Graphite
import           Control.Monad.Trans.Control
import           Data.Text.Prettyprint.Doc
import           Control.Monad.Catch
import           Control.Exception.Base         ( throwIO )
import           Time.Types                     ( toSeconds
                                                , Hours(..)
                                                )

newtype LoggingCurses a = LoggingCurses (LoggingT (Doc String) Curses a)
    deriving ( Applicative
             , Functor
             , Monad
             , MonadIO
             , MonadLog (Doc String)
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


runLoggingCurses :: LoggingCurses a -> IO a
runLoggingCurses (LoggingCurses lc) =
    withFile "aphrograph.log" WriteMode $ \logfile ->
        runCurses
            . withFDHandler defaultBatchingOptions logfile 0.4 80
            $ \logger -> runLoggingT lc logger

liftCurses :: Curses a -> LoggingCurses a
liftCurses = LoggingCurses <$> lift

updateFrame :: LoggingCurses ()
updateFrame = do
    liftCurses $ setEcho False
    window          <- liftCurses defaultWindow
    metrics         <- liftIO . getMetricsForPast . toSeconds $ Hours 100
    (width, height) <- liftCurses $ updateWindow window windowSize
    logMessage . pretty $ "width: " ++ show width ++ ", height: " ++ show height
    let (updates, logs) = unzip $ drawGraph (width, height) metrics
    logMessage . pretty $ "Number of updates: " ++ show (length updates)
    mapM_ (logMessage . pretty) logs
    liftCurses $ mapM_ (updateWindow window) updates
    liftCurses render
    liftCurses $ waitFor
        window
        (\ev -> ev == EventCharacter 'q' || ev == EventCharacter 'Q')

waitFor :: Window -> (Event -> Bool) -> Curses ()
waitFor w p = loop
  where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing  -> loop
            Just ev' -> unless (p ev') loop
