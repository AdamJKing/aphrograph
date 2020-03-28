{-# LANGUAGE GADTs #-}

module Display.Types where

import Formatting
import Text.Show

data Dimensions = Dims {width :: !Natural, height :: !Natural}
  deriving (Eq, Show)

data DisplayError = forall e. (Exception e) => ErrorDuringRender e

exception :: Exception e => Format r (e -> r)
exception = "{err=" % later (fromString . displayException) % "}"

instance Show DisplayError where
  show (ErrorDuringRender underlying) = (now "Error during render: " % exception) `formatToString` underlying

instance Exception DisplayError
