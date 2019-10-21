{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

module Display.Types where

import           Fmt
import           Text.Show


data Dimensions = Dims { width :: !Natural, height :: !Natural }
    deriving (Eq, Show)

data DisplayError = forall e. (Exception e) => ErrorDuringRender e

instance Show DisplayError where
  show (ErrorDuringRender underlying) = fmt $ "Error during render: err=" +|| underlying ||+ "."

instance Exception DisplayError where

