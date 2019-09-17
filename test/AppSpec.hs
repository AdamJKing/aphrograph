{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module AppSpec where

import           Test.Hspec                     ( Spec
                                                , describe
                                                )
import           Test.Hspec.QuickCheck          ( prop )
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           ArbitraryInstances             ( )
import           App
import           Network.HTTP.Req
import           CommonProperties
import           Graphite.Types

vanillaHttpException :: Gen HttpException
vanillaHttpException = VanillaHttpException <$> arbitrary

jsonParseException :: Gen HttpException
jsonParseException = JsonHttpException <$> arbitrary

spec :: Spec
spec = describe "App" $ describe "MonadHttp" $ do
  prop "captures all vanilla http exceptions as http errors" . monadic runAppProperty $ do
    err <- pick vanillaHttpException
    handleHttpExceptionApp err `shouldThrow` \case
      AppGraphiteError (HttpError _) -> return True
      _                              -> return False

  prop "captures all json parse exceptions as http errors" . monadic runAppProperty $ do
    err <- pick jsonParseException
    handleHttpExceptionApp err `shouldThrow` \case
      AppGraphiteError (ParsingError _) -> return True
      _ -> return False
