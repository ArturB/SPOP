{-|
Module      : Move
Description : Move datatype.
-}

module Move where

import Data.Aeson
import GHC.Generics
import Move.Direction
import Board
import Board.Coordinate

data Move = Move {
    direction :: Direction,
    start :: Coordinate
} deriving (Eq, Generic, Read, Show)

instance FromJSON Move
instance ToJSON Move

