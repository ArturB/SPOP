{-|
Module      : Board
Description : Game checkerboard.

The file includes game chackerboard datatype, represented as Map from Board.Coorinates to Board.Field. It also includes functions to update the board and AI to apply moves automatically. 

-}

module Board where

import           Board.Coordinate
import           Board.Field
import           Conduit
import           Data.Aeson
import qualified Data.ByteString.Lazy as ByteString
import           Data.Char
import           Data.List
import qualified Data.Map as Map
import           Data.Maybe
import           GHC.Generics
import           Move
import           Move.Status
import           Move.Direction

-- | Error message, displayed if initial position of the wolf is not in the range 1..4. 
invalidWolfPosition :: String
invalidWolfPosition = "Wolf position must be integer in range 1..4" 

-- | Checkerboard datatype
newtype Board = Board {
    board :: Map.Map Coordinate Field -- ^ Map from Board.Coordinate to Board.Field
} deriving (Eq, Generic)

-- | Print the board on the screen, line by line, with fields coordinates and with boundaries. 
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

-- | Return a initial board to start the game. 
init :: Int -- ^ Initial wolf position. Must be positive integer from 1 to 4 and denotes wolf position on the first (upper) row of the board, from left to right. 
     -> Board -- ^ Initial board state. 
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

-- | Checks if given move is valid on the board or is invalid for some reason. 
moveStatus :: Board  -- ^ Board state. 
           -> Move   -- ^ Move to check. 
           -> Status -- ^ Move validity. 
moveStatus (Board b) (Move s d)
       | b Map.! s == Empty = NothingToMove
       | s `move` d == Board.Coordinate.OutOfBoard = Move.Status.OutOfBoard
       | b Map.! (s `move` d) /= Empty = DestinationNotEmpty
       | b Map.! s == Sheep && vaxis d == Down = SheepCannotGoBack
       | otherwise = OK

-- | Infix equivalent of 'moveStatus'. 
(??) :: Board -> Move -> Status
(??) = moveStatus

-- | Execute a move on a board. 
apply :: Board -- ^ Initial board state. 
      -> Move  -- ^ Move to execute.
      -> Board -- ^ New board state. 
apply brd@(Board b) mv@(Move s d) = 
    let movedActor = b Map.! s
        destination = s |>> d
        b2 = Map.insert destination movedActor b
        b3 = Map.insert s Empty b2
    in  if brd ?? mv == OK then Board b3 else brd

-- | Infix equivalent of 'apply'. 
(>>>) :: Board -> Move -> Board
(>>>) = apply

-- | Return a wolf coordinates on the board. 
wolfCoord :: Board -> Coordinate
wolfCoord (Board b) = fst $ head $ Map.toList $ ( == Wolf) `Map.filter` b 

-- | Return a list of sheep coordinates on the board. 
sheepsCoords :: Board -> [Coordinate]
sheepsCoords (Board b) = fst <$> Map.toList ((== Sheep) `Map.filter` b)

-- | List of moves of the wolf, that are valid in the current board state. 
validWolfMoves :: Board -> [Move]
validWolfMoves brd = 
    let possibleMoves = Move (wolfCoord brd) <$> [DownLeft .. UpRight]
    in  (\m -> brd ?? m == OK) `filter` possibleMoves

-- | List of moves of the sheeps, that are valid in the current board state.  
validSheepsMoves :: Board -> [Move]
validSheepsMoves brd = 
    let possibleMovesCombinations = (\ sp d -> (sp, d)) <$> sheepsCoords brd <*> [UpLeft .. UpRight]
        possibleMoves = uncurry Move <$> possibleMovesCombinations
    in  (\m -> brd ?? m == OK) `filter` possibleMoves

-- | Calculates points, that wolf can use to rate its current situation in the game. With more points, the wolf situation is considered better. This function can be used as heuristics in mini-max algorithm and the game tree. 
{-| Changing the formula has great impact on the result of the game. Currently, wolf points are calculated as @points = wolf distance from upper board edge + 2 * mean distance from wolf to sheeps@
-}
wolfPoints :: Board -> Double
wolfPoints brd = 
    let wolfC = wolfCoord brd
        sheepsCs = sheepsCoords brd
        distFromUpperEdge = fromIntegral $ ord (show wolfC !! 1) - ord (pred '1')
        wolfSheepsAvgDist = sum (distance <$> [wolfC] <*> sheepsCs) / fromIntegral (length sheepsCs)
    in  distFromUpperEdge + 2 * wolfSheepsAvgDist

-- | Points that sheeps can use to rate their current situation in the game. Calculated as @-1 * 'wolfPoints'@. 
sheepsPoints :: Board -> Double
sheepsPoints brd = -1 * wolfPoints brd

-- | From list of possible moves, return the best (the one that leads to the state of the board with biggest amount of points).
bestMove :: (Board -> Double)   -- ^ A function used to rate the board state. May be either wolfPoints or sheepsPoints. 
         -> Board               -- ^ Current board state. 
         -> [Move]              -- ^ List of moves to analyze. May be empty, if no moves are possible (one side won the game). 
         -> Maybe (Move,Double) -- ^ The best move, alogside with its rating. May be Nothing, if list of moves to analyze is empty. 
bestMove points brd ms = 
    let moves = (\m -> (m, points (brd >>> m))) <$> ms
    in  if null moves then Nothing else Just $ foldl1' (\(m1,p1) (m2,p2) -> if p1 > p2 then (m1,p1) else (m2,p2)) moves

-- | Auxiliary function, calculates the best wolf move on the current board state. Applies 'wolfPoints' and 'validWolfMoves' to bestMove. 
{-| TODO: add recursive analyze of game tree. -}
bestWolfMove :: Int                 -- ^ Depth of game tree to analyze. 
             -> Board               -- ^ Current board state.
             -> Maybe (Move,Double) -- ^ The best wolf move, alogside with its rating. May be Nothing, if list of moves to analyze is empty. 
bestWolfMove 0 brd = bestMove wolfPoints brd $ validWolfMoves brd

-- | Auxiliary function, calculates the best sheeps move on the current board state. Applies 'sheepsPoints' and 'validSheepsMoves' to bestMove. 
{-| TODO: add recursive analyze of game tree -}
bestSheepMove :: Int                 -- ^ Depth of game tree to analyze. 
              -> Board               -- ^ Current board state. 
              -> Maybe (Move,Double) -- ^ The best wolf move, alogside with its rating. May be Nothing, if list of moves to analyze is empty. 
bestSheepMove 0 brd = bestMove sheepsPoints brd $ validSheepsMoves brd

-- | JSONize and save board to file
toFile :: Board  -- ^ Board state to save.
       -> String -- ^ File name. 
       -> IO ()
toFile (Board b) fileName = ByteString.writeFile fileName $ encode (Map.toList b)

-- | Read board state from file
fromFile :: String -- ^ File name. 
         -> IO Board
fromFile fileName = do
    bs <- ByteString.readFile fileName
    let bmap = Map.fromList $ fromJust $ decode bs
    return $ Board bmap

