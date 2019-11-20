{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PartialTypeSignatures #-}

module AppSpec where

import           Test.Hspec                     ( Spec
                                                , describe
                                                )
import           Test.Hspec.QuickCheck          ( prop )
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           ArbitraryInstances             ( )
import           App
import           App.Config
import           App.State                     as App
import           Graphite.Types                as Graphite
import           CommonProperties
import           Network.HTTP.Req              as Req

vanillaHttpException :: Gen HttpException
vanillaHttpException = VanillaHttpException <$> arbitrary

jsonParseException :: Gen HttpException
jsonParseException = JsonHttpException <$> arbitrary

noOpLogger :: App.Logger IO
noOpLogger = const pass

testConfig :: Config
testConfig = Config (GraphiteConfig (From "") Nothing "" (GraphiteUrl (http "localhost")))

spec :: Spec
spec = describe "App" $ describe "MonadHttp" $ do
  prop "captures all vanilla http exceptions as http errors"
    . monadic (idempotentIOProperty . runApp noOpLogger testConfig)
    $ do
        err <- pick vanillaHttpException
        shouldThrowMatching (handleHttpException err) $ \case
          (App.HttpError (Req.VanillaHttpException _)) -> property True
          _ -> property False

  prop "captures all json parse exceptions as http errors"
    . monadic (idempotentIOProperty . runApp noOpLogger testConfig)
    $ do
        err <- pick jsonParseException
        shouldThrowMatching (handleHttpException err) $ \case
          (App.HttpError (Req.JsonHttpException _)) -> property True
          _ -> property False
