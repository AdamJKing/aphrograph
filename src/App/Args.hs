{-# LANGUAGE TemplateHaskell #-}

module App.Args where

import Options.Applicative.Simple as Opt
import Control.Lens
import Graphite.Types hiding (value)

data Args =
    Args
    { _fromTime :: From
    , _toTime :: To
    , _targetArg :: Text
    , _graphiteUrl :: Text
    }
    deriving (Show,Eq)

makeLenses ''Args

fromTimeArgument :: Parser From
fromTimeArgument =
    strOption
    $ long "from" <> help "Represents the 'from' argument of the Graphite API."

toTimeArgument :: Parser To
toTimeArgument =
    strOption
    $ long "to" <> help "Represents the 'to' argument of the Graphite API."

targetArgument :: Parser Text
targetArgument =
    strOption $ long "target" <> help "The Graphite metric string."

graphiteUrlArgument :: Parser Text
graphiteUrlArgument =
    strOption
    $ long "graphite-url"
    <> help "The graphite host."
    <> showDefault
    <> value "localhost"
    <> metavar "GRAPHITE_HOST"

arguments :: Parser Args
arguments =
    Args <$> fromTimeArgument
    <*> toTimeArgument
    <*> targetArgument
    <*> graphiteUrlArgument

withCommandLineArguments :: (Args -> IO b) -> IO b
withCommandLineArguments f =
    let version = "0.1"
        title = "△phrograph"
        description = "A command-line viewer for graphite metrics."
    in do
           (args,()) <- simpleOptions version title description arguments empty
           f args
