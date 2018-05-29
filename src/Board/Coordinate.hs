{-|
Module      : Board.Coordinate
Description : Board fields coordinates
Datatype that contains valid coordinates of checkerboard fields. 
-}

module Board.Coordinate where

import           Data.Aeson
import           Data.Char
import           GHC.Generics
import           Move.Direction

-- | Checkerboard has 8x8 size. Only black fields are used, so there are total 32 valid board fields and its coordinates. Also, out of the board coordinate is added, as moving a wolf or sheep may sometimes result in getting out of the board edges. 
data Coordinate = A1 | A3 | A5 | A7 |
                  B2 | B4 | B6 | B8 |
                  C1 | C3 | C5 | C7 |
                  D2 | D4 | D6 | D8 |
                  E1 | E3 | E5 | E7 |
                  F2 | F4 | F6 | F8 |
                  G1 | G3 | G5 | G7 |
                  H2 | H4 | H6 | H8 | OutOfBoard
    deriving (Enum, Eq, Generic, Ord, Read, Show)

instance FromJSON Coordinate
instance ToJSON Coordinate

-- | Calculate a new coordinate, after executing a move in specified direction. 
move :: Coordinate -> Direction -> Coordinate
c `move` d = 
    let char  = head $ show c
        int   = show c !! 1
        char2 = if haxis d == Move.Direction.Left then pred char else succ char
        int2  = if vaxis d == Move.Direction.Up then pred int else succ int
        code2 = [char2, int2]
        validCodes = show <$> [A1 .. H8]
    in  if code2 `notElem` validCodes then OutOfBoard else (read code2 :: Coordinate)

-- | Infix equivalent of 'move'. 
(|>>) :: Coordinate -> Direction -> Coordinate
(|>>) = move

-- | Calculate euclidean distance between two fields. 
distance :: Coordinate -> Coordinate -> Double
distance c1 c2 = 
    let x1 = fromIntegral $ ord $ head $ show c1
        y1 = fromIntegral $ ord $ show c1 !! 1
        x2 = fromIntegral $ ord $ head $ show c2
        y2 = fromIntegral $ ord $ show c2 !! 1
    in sqrt $ (x2 - x1) ** 2 + (y2 - y1) ** 2

