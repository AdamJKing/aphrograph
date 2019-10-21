{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}

module AppSpec where

import           Test.Hspec                     ( Spec
                                                , describe
                                                )
import           Test.Hspec.QuickCheck          ( prop )
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           ArbitraryInstances             ( )
import           App.State
import           Network.HTTP.Req
import           CommonProperties
import           Graphite.Types

vanillaHttpException :: Gen HttpException
vanillaHttpException = VanillaHttpException <$> arbitrary

jsonParseException :: Gen HttpException
jsonParseException = JsonHttpException <$> arbitrary

spec :: Spec
spec = describe "App" $ describe "MonadHttp" $ do
  prop "captures all vanilla http exceptions as http errors" . monadicApp $ do
    err <- pick vanillaHttpException
    handleHttpException err `shouldThrow` \case
      AppGraphiteError _ -> return True
      _                  -> return False

  prop "captures all json parse exceptions as http errors" . monadicApp $ do
    err <- pick jsonParseException
    handleHttpException err `shouldThrow` \case
      AppGraphiteError (ParsingError _) -> return True
      _ -> return False
