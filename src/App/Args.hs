{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ExistentialQuantification #-}

module App.Args where

import           Options.Applicative.Simple    as Opt
import           Graphite.Types          hiding ( value )
import           Paths_aphrograph               ( version )
import           Data.Version                   ( showVersion )
import           Network.HTTP.Req
import           App.Config                    as App


fromTimeArgument :: Parser From
fromTimeArgument = strOption $ long "from" <> help
  "Represents the 'from' argument of the Graphite API. (see https://graphite-api.readthedocs.io/en/latest/)"

toTimeArgument :: Parser To
toTimeArgument = strOption $ long "to" <> help
  "Represents the 'to' argument of the Graphite API. (see https://graphite-api.readthedocs.io/en/latest/)"

targetArgument :: Parser Text
targetArgument =
  strOption $ long "target" <> help "The Graphite metric string. (see https://graphite-api.readthedocs.io/en/latest/)"

graphiteUrlArgument :: Parser GraphiteUrl
graphiteUrlArgument = option httpParser $ long "graphite-url" <> help "The graphite host." <> metavar "GRAPHITE_HOST"

httpParser :: ReadM GraphiteUrl
httpParser = maybeReader $ \input -> parseUrl (fromString input) <&> \case
  Right (httpsUrl, _) -> GraphiteUrl httpsUrl
  Left  (httpUrl , _) -> GraphiteUrl httpUrl

debugArgument :: Parser Bool
debugArgument = switch $ long "debug" <> hidden

arguments :: Parser App.Config
arguments = do
  fromTime    <- fromTimeArgument
  toTime      <- optional toTimeArgument
  targetArg   <- targetArgument
  graphiteUrl <- graphiteUrlArgument
  return (App.Config $ GraphiteConfig { .. })

withCommandLineArguments :: (App.Config -> IO b) -> IO b
withCommandLineArguments f =
  let version'    = showVersion version
      title       = "△phrograph"
      description = "A command-line viewer for graphite metrics."
  in  do
        (args, ()) <- simpleOptions version' title description arguments empty
        f args
