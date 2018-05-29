{-|
Module      : Move
Description : Move datatype.
-}

module Move where

import Data.Aeson
import GHC.Generics
import Move.Direction
import Board.Coordinate

data Move = Move {
    start :: Coordinate, 
    direction :: Direction
}  | AI {
    level :: Int
}deriving (Eq, Generic, Read, Show)

instance FromJSON Move
instance ToJSON Move


