{-|
Module      : Move.Direction
Description : Possible moving directions. 
-}

module Move.Direction where

import           Data.Aeson
import           GHC.Generics

-- | On a checkerboard, a move can be done diagonally: up or down and left or right. 
data Direction = DownLeft | DownRight | UpLeft | UpRight deriving (Enum, Eq, Generic, Ord, Read, Show)

instance FromJSON Direction
instance ToJSON Direction

-- | On vertical axis, a move can be done up or down. 
data VAxis = Down | Up    deriving (Enum, Eq, Generic, Ord, Read, Show)
-- | On horizontal axis, a move can be done left or right. 
data HAxis = Left | Right deriving (Enum, Eq, Generic, Ord, Read, Show)

-- | Return vertical direction of the move. 
vaxis :: Direction -> VAxis
vaxis DownLeft  = Down
vaxis DownRight = Down
vaxis UpLeft    = Up
vaxis UpRight   = Up

-- | Return horizontal direction of the move. 
haxis :: Direction -> HAxis
haxis DownLeft  = Move.Direction.Left
haxis DownRight = Move.Direction.Right
haxis UpLeft    = Move.Direction.Left
haxis UpRight   = Move.Direction.Right

