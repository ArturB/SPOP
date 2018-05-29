{-|
Module      : Board.Coordinate
Description : Possible board coordinates
-}

module Board.Coordinate where

import           Data.Aeson
import           GHC.Generics
import           Move.Direction

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

move :: Coordinate -> Direction -> Coordinate
c `move` d = 
    let char  = head $ show c
        int   = show c !! 1
        char2 = if haxis d == Move.Direction.Left then pred char else succ char
        int2  = if vaxis d == Move.Direction.Up then pred int else succ int
        code2 = [char2, int2]
        validCodes = show <$> [A1 .. H8]
    in  if code2 `notElem` validCodes then OutOfBoard else (read code2 :: Coordinate)

(|>>) :: Coordinate -> Direction -> Coordinate
(|>>) = move