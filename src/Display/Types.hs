{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

module Display.Types where

import           Fmt
import           Text.Show


data Dimensions i = Dimensions { width :: !i, height :: !i }
    deriving (Eq, Show, Functor)

dim :: (i, i) -> Dimensions i
dim (width, height) = Dimensions { .. }

data DisplayError = forall e. (Exception e) => ErrorDuringRender e

instance Show DisplayError where
  show (ErrorDuringRender underlying) = fmt $ "Error during render: err=" +|| underlying ||+ "."

instance Exception DisplayError where

