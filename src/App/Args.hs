{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module App.Args where

import App.Config as App
import Control.Lens.Prism
import Control.Lens.Setter
import Data.Version (showVersion)
import Formatting
import Graphite.Types hiding (value)
import Network.HTTP.Req
import Options.Applicative.Simple as Opt
import Paths_aphrograph (version)
import Text.URI

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

debugArgument :: Parser Bool
debugArgument = switch $ long "debug" <> hidden

arguments :: Parser App.Config
arguments = do
  _fromTime <- fromTimeArgument
  _toTime <- optional toTimeArgument
  _targetArg <- targetArgument
  _graphiteUrl <- graphiteUrlArgument
  return (App.Config $ GraphiteConfig {..})

withCommandLineArguments :: (App.Config -> IO b) -> IO b
withCommandLineArguments f =
  let version' = showVersion version
      title = "△phrograph"
      description = "A command-line viewer for graphite metrics."
   in do
        (args, ()) <- simpleOptions version' title description arguments empty
        f args
