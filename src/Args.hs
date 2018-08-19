{-# LANGUAGE QuasiQuotes, FlexibleContexts #-}

module Args where

import           Data.Int
import           Data.Hourglass.Types
import           Text.Regex.PCRE.Heavy

parseTime :: String -> Maybe Seconds
parseTime str = case (scan [re|([0-9]+)([smhd])|] str) of
  [(_, [time, unit])] -> case unit of
    "s" -> Just $ Seconds (read time :: Int64)
    "m" -> Just . toSeconds $ Minutes (read time :: Int64)
    "h" -> Just . toSeconds $ Hours (read time :: Int64)
    "d" -> Just . toSeconds $ Hours (read time :: Int64) * 24
  _ -> Nothing
