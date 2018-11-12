module Args where

import           Data.Char
import           Data.Hourglass.Types

data AppArgs = AppArgs {
  _time :: Seconds
  , _target :: String
} deriving (Show, Eq)

class ArgumentParser arg where
  parseArg :: String -> Either String arg

instance ArgumentParser Seconds where
  parseArg input = case parseTime' input "" "" of
    (time, "d") -> Right . toSeconds . Hours $ read time * 24
    (time, "h") -> Right . toSeconds . Hours $ read time
    (time, "m") -> Right . toSeconds . Minutes $ read time
    (time, "s") -> Right . Seconds $ read time
    _           -> Left $ "Invalid time. (" ++ input ++ ")"
    where
      parseTime' :: String -> String -> String -> (String, String)
      parseTime' "" time unit = (time, unit)
      parseTime' (a : as) time unit | isDigit a  = parseTime' as (time ++ [a]) unit
                                    | isLetter a = parseTime' as time (unit ++ [a])
                                    | otherwise  = ("", "")


parseAppArgs :: [String] -> Either String AppArgs
parseAppArgs [targetS, timeS] = do
  time <- parseArg timeS
  return $ AppArgs { _time = time, _target = targetS }
parseAppArgs _ = Left usageMessage

usageMessage :: String
usageMessage = "aphrograph-exe $TARGET $TIME"
