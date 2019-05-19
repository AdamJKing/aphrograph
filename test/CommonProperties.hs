{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module CommonProperties where

import           Control.Monad.Except    hiding ( runExceptT )
import           Test.QuickCheck.Property
import           ArbitraryInstances             ( )
import           Test.QuickCheck
import           Test.Hspec
import           Test.HUnit.Base                ( assertFailure )
import           Graphite.Types
import           App
import           App.Args                      as App
import           Control.Monad.Log
import           Fmt                     hiding ( build )


newtype UniqueList a = Unique { getUnique :: [a] }
  deriving (Show, Eq, Foldable)

instance (Arbitrary a, Eq a) => Arbitrary (UniqueList a) where
  arbitrary = sized $ fmap Unique . build
   where
    build 0  = return []
    build n' = do
      as <- build (n' - 1)
      a  <- arbitrary `suchThat` (`notElem` as)
      return $ a : as

range :: (Ord a, Arbitrary a) => Gen (a, a)
range = do
  x <- arbitrary
  y <- arbitrary `suchThat` (/= x)
  return (min x y, max x y)

-- Temporary until QuickCheck 2.13
instance Testable prop => Testable (Maybe prop) where
  property = property . liftMaybe
   where
    liftMaybe Nothing     = property Discard
    liftMaybe (Just prop) = property prop

daysFrom :: Word16 -> Time -> [Time]
daysFrom n = take (fromIntegral n + 1) . iterate (+ 86400)

data DummyComponent = DummyComponent deriving (Eq, Show)

newtype TestIO a = TestIO (ReaderT App.Args (DiscardLoggingT Text (ExceptT AppError IO )) a)
    deriving (Functor, Applicative, Monad, MonadLog Text, MonadReader App.Args, MonadIO, MonadError AppError)

-- data TestM a = TestM {
--     args :: Maybe App.Args,
--     test :: ReaderT App.Args ( DiscardLoggingT Text (Either AppError)) a
-- }

-- instance Applicative TestM where

-- instance Monad TestM where

-- instance MonadError AppError TestM where

defaultArgs :: App.Args
defaultArgs = App.Args { _fromTime    = From "-10h"
                       , _toTime      = To "0"
                       , _targetArg   = "example.target"
                       , _graphiteUrl = "graphite.url.com"
                       }

-- executeTest :: TestM a -> Either AppError a
-- executeTest TestM {..} =
--   discardLogging . runReaderT test $ fromMaybe defaultArgs args

arbitraryTestIO :: (Arbitrary a) => TestIO a
arbitraryTestIO = liftIO $ generate arbitrary

-- shouldFailWith :: Show a => TestM a -> AppError -> Expectation
-- shouldFailWith test expectedError = case executeTest test of
--   Left err -> err `shouldBe` expectedError
--   Right unexpected ->
--     assertFailure
--       $   fmt "test succeeded unexpectedly with "
--       +|| unexpected
--       ||+ "."

instance Testable t => Testable ( TestIO t ) where
  property (TestIO t) = idempotentIOProperty $ do
    args   <- generate (applyArbitrary4 App.Args)
    result <- runExceptT . discardLogging $ usingReaderT args t
    case result of
      Right a -> return $ property a
      Left  _ -> return $ property failed
