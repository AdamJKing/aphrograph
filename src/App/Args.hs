{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}

module App.Args where

import           Options.Applicative.Simple    as Opt
import           Control.Lens
import           Graphite.Types          hiding ( value )
import           Paths_aphrograph               ( version )
import           Data.Version                   ( showVersion )
import           Network.HTTP.Req


data Args =
    Args
    { _fromTime :: From
    , _toTime :: Maybe To
    , _targetArg :: Text
    , _graphiteUrl :: GraphiteUrl
    , _debugMode :: Bool
   } deriving (Show, Eq)

makeLenses ''Args

fromTimeArgument :: Parser From
fromTimeArgument =
  strOption
    $  long "from"
    <> help
         "Represents the 'from' argument of the Graphite API. (see https://graphite-api.readthedocs.io/en/latest/)"

toTimeArgument :: Parser To
toTimeArgument =
  strOption
    $  long "to"
    <> help
         "Represents the 'to' argument of the Graphite API. (see https://graphite-api.readthedocs.io/en/latest/)"

targetArgument :: Parser Text
targetArgument =
  strOption
    $  long "target"
    <> help
         "The Graphite metric string. (see https://graphite-api.readthedocs.io/en/latest/)"

graphiteUrlArgument :: Parser GraphiteUrl
graphiteUrlArgument =
  option httpParser
    $  long "graphite-url"
    <> help "The graphite host."
    <> metavar "GRAPHITE_HOST"

httpParser :: ReadM GraphiteUrl
httpParser = maybeReader $ \input -> parseUrl (fromString input) <&> \case
  Right (httpsUrl, _) -> GraphiteUrl httpsUrl
  Left  (httpUrl , _) -> GraphiteUrl httpUrl

debugArgument :: Parser Bool
debugArgument = switch $ long "debug" <> hidden

arguments :: Parser Args
arguments =
  Args
    <$> fromTimeArgument
    <*> optional toTimeArgument
    <*> targetArgument
    <*> graphiteUrlArgument
    <*> debugArgument

withCommandLineArguments :: (Args -> IO b) -> IO b
withCommandLineArguments f =
  let version'    = showVersion version
      title       = "△phrograph"
      description = "A command-line viewer for graphite metrics."
  in  do
        (args, ()) <- simpleOptions version' title description arguments empty
        f args
