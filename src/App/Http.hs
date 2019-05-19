module App.Http where

import           Control.Monad.Log
import           Control.Monad.Except
import           Control.Monad.Catch
import           Control.Lens            hiding ( from
                                                , to
                                                )
import qualified Data.Aeson                    as JSON
import qualified Data.Aeson.Types              as JSONT
import qualified Network.Wreq                  as Wreq
import           Network.HTTP.Client            ( HttpException(..) )
import qualified Data.Vector                   as V
import           Graphite.Types
import           Fmt
import           App.Args
import           App


getWith :: Wreq.Options -> String -> App (Wreq.Response LByteString)
getWith = liftIO . Wreq.getWith
