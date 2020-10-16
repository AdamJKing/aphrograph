{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module App.Args where

import qualified App.Config as App
import Control.Lens.Prism (_Left)
import Control.Lens.Setter (over)
import Data.Time (TimeZone, getCurrentTimeZone)
import Data.Version (showVersion)
import Formatting (formatToString, shown, (%))
import Graphite.Types (From, GraphiteUrl (..), To)
import Network.HTTP.Req (useURI)
import Options.Applicative.Simple as Opt
  ( ParseError (ErrorMsg),
    Parser,
    ReadM,
    auto,
    eitherReader,
    help,
    long,
    metavar,
    option,
    readerAbort,
    simpleOptions,
    strOption,
  )
import Paths_aphrograph (version)
import Text.URI (URI, mkURI)

fromTimeArgument :: Parser From
fromTimeArgument =
  strOption $
    long "from"
      <> help
        "Represents the 'from' argument of the Graphite API. (see https://graphite-api.readthedocs.io/en/latest/)"

toTimeArgument :: Parser To
toTimeArgument =
  strOption $
    long "to"
      <> help
        "Represents the 'to' argument of the Graphite API. (see https://graphite-api.readthedocs.io/en/latest/)"

targetArgument :: Parser Text
targetArgument =
  strOption $ long "target" <> help "The Graphite metric string. (see https://graphite-api.readthedocs.io/en/latest/)"

graphiteUrlArgument :: Parser GraphiteUrl
graphiteUrlArgument = option httpParser $ long "graphite-url" <> help "The graphite host." <> metavar "GRAPHITE_HOST"

uriParser :: ReadM URI
uriParser = eitherReader $ over _Left displayException . mkURI . fromString

httpParser :: ReadM GraphiteUrl
httpParser = do
  uri <- uriParser
  case useURI uri of
    Nothing -> readerAbort (ErrorMsg (formatToString ("Unknown protocol " % shown) uri))
    Just (Right (httpsUrl, _)) -> return $ GraphiteUrl httpsUrl
    Just (Left (httpUrl, _)) -> return $ GraphiteUrl httpUrl

timezoneArgument :: Parser TimeZone
timezoneArgument = option auto $ long "timezone" <> help "Timezone used to present the graph (Defaults to system timezone)"

arguments :: TimeZone -> Parser App.Config
arguments defaultTimezone = do
  _fromTime <- fromTimeArgument
  _toTime <- optional toTimeArgument
  _targetArg <- targetArgument
  _graphiteUrl <- graphiteUrlArgument
  _timezone <- timezoneArgument <|> pure defaultTimezone

  return (App.Config (App.GraphiteConfig {..}) _timezone)

withCommandLineArguments :: (App.Config -> IO b) -> IO b
withCommandLineArguments f =
  let version' = showVersion version
      title = "△phrograph"
      description = "A command-line viewer for graphite metrics."
   in do
        timezone <- getCurrentTimeZone
        (args, ()) <- simpleOptions version' title description (arguments timezone) empty
        f args
