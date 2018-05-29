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

data VAxis = Down | Up    deriving (Enum, Eq, Generic, Ord, Read, Show)
data HAxis = Left | Right deriving (Enum, Eq, Generic, Ord, Read, Show)

vaxis :: Direction -> VAxis
vaxis DownLeft  = Down
vaxis DownRight = Down
vaxis UpLeft    = Up
vaxis UpRight   = Up

haxis :: Direction -> HAxis
haxis DownLeft  = Move.Direction.Left
haxis DownRight = Move.Direction.Right
haxis UpLeft    = Move.Direction.Left
haxis UpRight   = Move.Direction.Right

