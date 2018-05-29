{-|
Module      : Board
Description : Game checkboard.
-}

module Board where

import Data.Char
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
    let wolfC = case wolfPos of
            1 -> A1
            2 -> C1
            3 -> E1
            4 -> G1 
            _ -> error invalidWolfPosition
        maplist = (\c -> if c == wolfC then (c,Wolf)
                       else if c `elem` [B8, D8, F8, H8] then (c,Sheep)
                       else (c,Empty)) <$> [A1 .. H8]
    in Board $ Map.fromList maplist

moveStatus :: Board -> Move -> Status
moveStatus (Board b) (Move s d)
       | b Map.! s == Empty = NothingToMove
       | s `move` d == Board.Coordinate.OutOfBoard = Move.Status.OutOfBoard
       | b Map.! (s `move` d) /= Empty = DestinationNotEmpty
       | b Map.! s == Sheep && vaxis d == Down = SheepCannotGoBack
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

wolfCoord :: Board -> Coordinate
wolfCoord (Board b) = fst $ head $ Map.toList $ ( == Wolf) `Map.filter` b 

sheepsCoords :: Board -> [Coordinate]
sheepsCoords (Board b) = fst <$> Map.toList ((== Sheep) `Map.filter` b)

validWolfMoves :: Board -> [Move]
validWolfMoves brd = 
    let possibleMoves = Move (wolfCoord brd) <$> [DownLeft .. UpRight]
    in  (\m -> brd ?? m == OK) `filter` possibleMoves

validSheepsMoves :: Board -> [Move]
validSheepsMoves brd = 
    let possibleMovesCombinations = (\ sp d -> (sp, d)) <$> sheepsCoords brd <*> [UpLeft .. UpRight]
        possibleMoves = uncurry Move <$> possibleMovesCombinations
    in  (\m -> brd ?? m == OK) `filter` possibleMoves

wolfPoints :: Board -> Double
wolfPoints brd = 
    let wolfC = wolfCoord brd
        sheepsCs = sheepsCoords brd
        distFromUpperEdge = fromIntegral $ ord (show wolfC !! 1) - ord (pred '1')
        wolfSheepsAvgDist = sum (distance <$> [wolfC] <*> sheepsCs) / fromIntegral (length sheepsCs)
    in  distFromUpperEdge + 2 * wolfSheepsAvgDist

sheepsPoints :: Board -> Double
sheepsPoints brd = -1 * wolfPoints brd

bestMove :: (Board -> Double) -> Board -> [Move] -> Maybe (Move,Double)
bestMove points brd ms = 
    let moves = (\m -> (m, points (brd >>> m))) <$> ms
    in  if null moves then Nothing else Just $ foldl1' (\(m1,p1) (m2,p2) -> if p1 > p2 then (m1,p1) else (m2,p2)) moves

bestWolfMove :: Int -> Board -> Maybe (Move,Double)
bestWolfMove 0 brd = bestMove wolfPoints brd $ validWolfMoves brd
        
bestSheepMove :: Int -> Board -> Maybe (Move,Double)
bestSheepMove 0 brd = bestMove sheepsPoints brd $ validSheepsMoves brd

