{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphite where

import           Control.Monad.Log
import           Control.Monad.Except
import           Control.Monad.Catch
import           Control.Lens            hiding ( from
                                                , to
                                                )
import qualified Data.Aeson                    as JSON
import qualified Data.Aeson.Types              as JSONT
import           Network.Wreq                  as Wreq
import           Network.HTTP.Client            ( HttpException(..) )
import qualified Data.Vector                   as V
import           Graphite.Types
import           Fmt
import           App.Args
import           App


executeGraphiteRequest
  :: (MonadError AppError m, MonadIO m)
  => IO (Response LByteString)
  -> m (GraphiteResponse [DataPoint])
executeGraphiteRequest req =
  liftIO
    $ catch (req <&> buildMetrics . view responseBody)
    $ \(e :: HttpException) -> throwM (GraphiteUnavailable $ displayException e)
 where
  buildMetrics body = GraphiteResponse $ case parseMetricTimeSeries body of
    Right metrics -> metrics
    Left  err     -> throwM . AppGraphiteError $ GraphiteResponseError err

class MonadGraphite m where
  getMetrics :: RenderRequest -> m (GraphiteResponse [DataPoint])

instance MonadGraphite App where
  getMetrics RenderRequest {..} = do
    logMessage "Making a call to graphite."
    let parameters = constructQueryParams target from to
    url        <- views graphiteUrl buildUrl
    datapoints <- executeGraphiteRequest (getWith parameters url)
    logDataSize datapoints
    return datapoints
   where
    buildUrl graphiteHost = fmt $ "http://" +| graphiteHost |+ "/render"
    logDataSize (GraphiteResponse dps) =
      logMessage $ fmt "Size of returned data was: " +| length dps |+ "."

constructQueryParams :: Text -> From -> To -> Options
constructQueryParams target (From from) (To to) = set
  params
  [("target", target), ("from", from), ("to", to), ("format", "json")]
  defaults

parseMetricTimeSeries :: LByteString -> Either String [DataPoint]
parseMetricTimeSeries respBody = do
  strResp <- JSON.eitherDecode respBody
  JSONT.parseEither parseDataPoints strResp

 where
  unwrapArray arr = guard (not (V.null arr)) >> return (V.unsafeHead arr)
  parseDataPoints body = do
    unwrapped <- JSON.withArray "Array Wrapper" unwrapArray body
    JSON.withObject "Datapoints" (`JSONT.parseField` "datapoints") unwrapped

