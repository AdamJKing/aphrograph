{-# LANGUAGE QuasiQuotes, FlexibleContexts #-}

module Args where

import           Control.Monad.IO.Class
import           Data.Int
import           Data.Hourglass.Types
import           Text.Regex.PCRE.Heavy
import qualified System.Environment            as Env

data AppArgs = AppArgs {
  _time :: Seconds
  , _target :: String
}

getAppArgs :: (MonadIO m) => m AppArgs
getAppArgs = liftIO Env.getArgs >>= extract
 where
  extract [target, time] = do
    t <- case parseTime time of
      Just t  -> return t
      Nothing -> printUsage
    return $ AppArgs {_time = t, _target = target}
  extract _ = printUsage
  printUsage = error "Usage: aphrograph $TARGET $TIME"


parseTime :: String -> Maybe Seconds
parseTime str = case scan [re|([0-9]+)([smhd])|] str of
  [(_, [time, unit])] -> case unit of
    "s" -> Just $ Seconds (read time :: Int64)
    "m" -> Just . toSeconds $ Minutes (read time :: Int64)
    "h" -> Just . toSeconds $ Hours (read time :: Int64)
    "d" -> Just . toSeconds $ Hours (read time :: Int64) * 24
    _   -> Nothing
  _ -> Nothing
