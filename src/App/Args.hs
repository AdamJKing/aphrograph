{-# LANGUAGE TemplateHaskell #-}

module App.Args where

import           Options.Applicative.Simple
import           Data.Char                     as Char
import           Fmt
import           Data.Hourglass.Types
import           Control.Lens

data Args = Args {
  _timeArg :: Seconds
  , _targetArg :: Text
} deriving (Show, Eq)

makeLenses ''Args

readTime :: String -> Either String Seconds
readTime input = do
  let (time, unit) = splitInput input
  timeAsInt <- readEither' time
  case unit of
    "d" -> return $ toSeconds (Hours (timeAsInt * 24))
    "h" -> return $ toSeconds (Hours timeAsInt)
    "m" -> return $ toSeconds (Minutes timeAsInt)
    "s" -> return $ Seconds timeAsInt
    _   -> fail ("Invalid time. (" +| input |+ ")")
 where
  takeDigits  = takeWhile Char.isDigit
  dropDigits  = dropWhile Char.isDigit
  splitInput  = (,) <$> takeDigits <*> dropDigits
  readEither' = first toString . readEither

timeArgument :: Parser Seconds
timeArgument = option (eitherReader readTime) $ long "time" <> help
  "The timespan to search in (from now)."

targetArgument :: Parser Text
targetArgument =
  strOption $ long "target" <> help "The Graphite metric string."

arguments :: Parser Args
arguments = Args <$> timeArgument <*> targetArgument


withCommandLineArguments :: (Args -> IO b) -> IO b
withCommandLineArguments f =
  let version     = "0.1"
      title       = "△phrograph"
      description = "A command-line viewer for graphite metrics."
  in  do
        (args, ()) <- simpleOptions version title description arguments empty
        f args
