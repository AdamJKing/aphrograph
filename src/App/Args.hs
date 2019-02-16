{-# LANGUAGE TemplateHaskell #-}

module App.Args where

import           Data.Char
import           Control.Arrow
import           Fmt
import           Data.Hourglass.Types
import           Data.Text                     as T
import           Relude.Extra.Tuple
import           Control.Lens

data Args = Args {
  _timeArg :: Seconds
  , _targetArg :: Text
} deriving (Show, Eq)

class ArgumentParser arg where
  parseArg :: Text -> Either Text arg

instance ArgumentParser Seconds where
  parseArg input = case parseTime' input of
    (time, "d") -> toSeconds . Hours . (* 24) <$> readEither time
    (time, "h") -> toSeconds . Hours <$> readEither time
    (time, "m") -> toSeconds . Minutes <$> readEither time
    (time, "s") -> Seconds <$> readEither time
    _           -> Left $ "Invalid time. (" +| input |+ ")"
    where parseTime' = (T.takeWhile isDigit *** T.dropWhile isDigit) . dupe


parseAppArgs :: [Text] -> Either Text Args
parseAppArgs [targetS, timeS] = do
  time <- parseArg timeS
  return $ Args { _timeArg = time, _targetArg = targetS }
parseAppArgs _ = Left usageMessage

usageMessage :: Text
usageMessage = "aphrograph-exe $TARGET $TIME"

makeLenses ''Args
