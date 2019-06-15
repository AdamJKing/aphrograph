{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import           Graphics.Vty
import qualified Data.Aeson as JSON
import           Network.Wai.Handler.Warp
import           Servant
import           Test.QuickCheck
import           Test.QuickCheck.Instances.Text ()
import           Test.QuickCheck.Instances.Scientific ()
import           Test.Hspec
import           System.Posix.IO (openFd, OpenMode(ReadWrite), defaultFileFlags
                                , fdWrite)
import           System.Posix.Temp (mkstemp)
import           System.Posix.Terminal (openPseudoTerminal)
import           System.IO
import           System.Posix.Types (Fd)
import           System.Environment (withArgs)
import           Aphrograph (aphrograph)
import           Control.Concurrent.Async
import           Control.Concurrent

newtype MetricResponse = MetricResponse JSON.Value
  deriving (JSON.ToJSON)

instance Arbitrary MetricResponse where
  arbitrary = do
    target <- JSON.String <$> arbitrary
    datapoints <- jsonArray <$> listOf genDatapoint
    return . MetricResponse
      $ JSON.object [("target", target), ("datapoints", datapoints)]
    where
      jsonArray = JSON.Array . fromList

      genDatapoint = do
        value <- JSON.Number <$> arbitrary
        time <- JSON.Number . fromInteger <$> arbitrary
        return . jsonArray $ [value, time]

type GraphiteRenderApi = "succeeding" :> "render" :> Get '[JSON] MetricResponse
  :<|> "failing" :> "render" :> Get '[JSON] MetricResponse

api :: Proxy GraphiteRenderApi
api = Proxy

server :: Server GraphiteRenderApi
server = succesfulApiCall :<|> failingApiCall
  where
    succesfulApiCall = liftIO $ generate (arbitrary @MetricResponse)

    failingApiCall = throwError err500

testConfig :: Fd -> FilePath -> IO Config
testConfig input outputPath = do
  output <- openFd outputPath ReadWrite Nothing defaultFileFlags
  return $ defaultConfig { inputFd = pure input, outputFd = pure output }

withWebserver :: IO b -> IO b
withWebserver = withAsync (run 8080 (serve api server)) . const

main :: IO ()
main = withWebserver
  $ do
    (toInput, input) <- openPseudoTerminal
    (outputPath, outputFile) <- mkstemp "test-output"
    vty <- mkVty =<< testConfig input outputPath
    aph <- async
      . withArgs ["--from", "1h", "--target", "some.test.target"]
      $ aphrograph vty
    _ <- fdWrite toInput "q"
    _ <- race (threadDelay 3000000) (wait aph)
    stuff <- hGetContents outputFile
    hspec
      $ describe "Test"
      $ it "should have stuff"
      $ stuff `shouldContain` "line"
