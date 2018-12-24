module Args where

import           Data.Char
import           Control.Arrow
import           Fmt
import           Data.Hourglass.Types
import           Data.Text                     as T
import           Relude.Extra.Tuple

data AppArgs = AppArgs {
  _time :: Seconds
  , _target :: Text
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


parseAppArgs :: [Text] -> Either Text AppArgs
parseAppArgs [targetS, timeS] = do
  time <- parseArg timeS
  return $ AppArgs { _time = time, _target = targetS }
parseAppArgs _ = Left usageMessage

usageMessage :: Text
usageMessage = "aphrograph-exe $TARGET $TIME"
