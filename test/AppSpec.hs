{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuantifiedConstraints #-}

module AppSpec where

import           Test.Hspec                    as HS
import           Test.Hspec.QuickCheck          ( prop )
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Prelude                 hiding ( null )
import           ArbitraryInstances             ( )
import           App
import           Network.HTTP.Req               ( handleHttpException
                                                , HttpException
                                                )
import           Control.Monad.Error            ( catchError )
import           CommonProperties

spec :: HS.Spec
spec = describe "App" $ do
    describe "MonadHttp"
        $ appProp "captures all http exceptions as AppErrors"
        $ do
              err <- pick (arbitrary @HttpException)
              run
                  $ catchError (handleHttpException err)
                  $ \(appError :: AppError) -> return True

        -- do
        --       catchError (handleHttpException err) $ \_ -> fail "Expected exception."
        --       assert True

    describe "AppError" $ do
        prop "displays the underlying exception" $ \(SomeException e) ->
            displayException (AppError e) === displayException e

        prop "show is the same as display exception"
            $ \(err :: AppError) -> show err === displayException err
