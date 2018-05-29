{-|
Module      : Move
Description : Move datatype.
Datatype that represnts a move that player can do. 
-}

module Move where

import Data.Aeson
import GHC.Generics
import Move.Direction
import Board.Coordinate

-- | Move datatype. 
data Move = Move {         
    start :: Coordinate,   -- ^ Coordinate of the actor (wolf or sheep) to move. 
    direction :: Direction -- ^ Move direction. 
}  | AI {
    level :: Int -- ^ AI move, contrary to user move, contains AI level (basically a game tree depth to analyze). 
} deriving (Eq, Generic, Read, Show)

instance FromJSON Move 
instance ToJSON Move


