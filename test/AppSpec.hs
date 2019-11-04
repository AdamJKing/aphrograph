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
import           Network.HTTP.Req
import           CommonProperties
import           Graphite.Types
import           Graphite                       ( runGraphite
                                                , GraphiteT
                                                )

vanillaHttpException :: Gen HttpException
vanillaHttpException = VanillaHttpException <$> arbitrary

jsonParseException :: Gen HttpException
jsonParseException = JsonHttpException <$> arbitrary

type MockGraphite = PropertyM (GraphiteT IO)

spec :: Spec
spec = describe "App" $ describe "MonadHttp" $ do
  prop "captures all vanilla http exceptions as http errors" . monadic (idempotentIOProperty . runGraphite) $ do
    err <- pick vanillaHttpException
    handleHttpException err `shouldThrowMatching` _HttpError

  prop "captures all json parse exceptions as http errors" . monadic (idempotentIOProperty . runGraphite) $ do
    err <- pick jsonParseException
    handleHttpException err `shouldThrowMatching` _ParsingError
