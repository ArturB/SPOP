{-|
Module      : Board.Coordinate
Description : Possible board coordinates
-}

module Board.Coordinate where

import           Data.Aeson
import           GHC.Generics

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

