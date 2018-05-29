{-|
Module      : Board
Description : Game checkboard.
-}

module Board where

import           Data.Aeson
import qualified Data.Map as Map
import           GHC.Generics
import Board.Coordinate
import Board.Field
import Data.List
import Move
import Move.Status
import Move.Direction

invalidWolfPosition :: String
invalidWolfPosition = "Wolf position must be integer in range 1..4" 

newtype Board = Board {
    board :: Map.Map Coordinate Field
} deriving (Eq, Generic)

instance Show Board where
    show (Board b) = 
        let upperLine = "    -----------------"
            line0 = "     " ++ "A B C D E F G H"
            line1 = " 1 | "   ++ "   " `intercalate` [show (b Map.! A1), show (b Map.! C1),  show (b Map.! E1), show (b Map.! G1)] ++ "   |"
            line2 = " 2 |   " ++ "   " `intercalate` [show (b Map.! B2), show (b Map.! D2),  show (b Map.! F2), show (b Map.! H2)] ++ " |"
            line3 = " 3 | "   ++ "   " `intercalate` [show (b Map.! A3), show (b Map.! C3),  show (b Map.! E3), show (b Map.! G3)] ++ "   |"
            line4 = " 4 |   " ++ "   " `intercalate` [show (b Map.! B4), show (b Map.! D4),  show (b Map.! F4), show (b Map.! H4)] ++ " |"
            line5 = " 5 | "   ++ "   " `intercalate` [show (b Map.! A5), show (b Map.! C5),  show (b Map.! E5), show (b Map.! G5)] ++ "   |"
            line6 = " 6 |   " ++ "   " `intercalate` [show (b Map.! B6), show (b Map.! D6),  show (b Map.! F6), show (b Map.! H6)] ++ " |"
            line7 = " 7 | "   ++ "   " `intercalate` [show (b Map.! A7), show (b Map.! C7),  show (b Map.! E7), show (b Map.! G7)] ++ "   |"
            line8 = " 8 |   " ++ "   " `intercalate` [show (b Map.! B8), show (b Map.! D8),  show (b Map.! F8), show (b Map.! H8)] ++ " |"
        in "\n" ++ line0 ++ "\n" ++ upperLine ++ "\n" ++ line1 ++ "\n" ++ line2 ++ "\n" ++ line3 ++ "\n" ++ line4 ++ "\n" ++ line5 ++ "\n" ++ line6 ++ "\n" ++ line7 ++ "\n" ++ line8 ++ "\n" ++ upperLine ++ "\n"

init :: Int -> Board
init wolfPos = 
    let wolfCoord = case wolfPos of
            1 -> A1
            2 -> C1
            3 -> E1
            4 -> G1 
            _ -> error invalidWolfPosition
        maplist = (\c -> if c == wolfCoord then (c,Wolf)
                       else if c `elem` [B8, D8, F8, H8] then (c,Sheep)
                       else (c,Empty)) <$> [A1 .. H8]
    in Board $ Map.fromList maplist

moveStatus :: Board -> Move -> Status
moveStatus (Board b) (Move s d)
       | b Map.! s == Empty = NothingToMove
       | b Map.! (s `move` d) /= Empty = DestinationNotEmpty
       | b Map.! s == Sheep && vaxis d == Down = SheepCannotGoBack
       | s `move` d == Board.Coordinate.OutOfBoard = Move.Status.OutOfBoard
       | otherwise = OK

(??) :: Board -> Move -> Status
(??) = moveStatus

apply :: Board -> Move -> Board
apply brd@(Board b) mv@(Move s d) = 
    let movedActor = b Map.! s
        destination = s |>> d
        b2 = Map.insert destination movedActor b
        b3 = Map.insert s Empty b2
    in  if brd ?? mv == OK then Board b3 else brd

(>>>) :: Board -> Move -> Board
(>>>) = apply

