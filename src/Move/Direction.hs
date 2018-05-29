{-|
Module      : Move.Direction
Description : Each move has one of four possible directions.
-}

module Move.Direction where

import           Data.Aeson
import           GHC.Generics

data Direction = DownLeft | DownRight | UpLeft | UpRight deriving (Enum, Eq, Generic, Ord, Read, Show)

instance FromJSON Direction
instance ToJSON Direction

