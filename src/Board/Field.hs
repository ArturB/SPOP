{-|
Module      : Board.Field
Description : Possible states of board field.
-}

module Board.Field where

import           Data.Aeson
import           GHC.Generics

data Field = Empty | Sheep | Wolf deriving (Enum, Eq, Ord, Generic)

instance FromJSON Field
instance ToJSON Field

instance Show Field where
    show Empty = "."
    show Sheep = "o"
    show Wolf  = "x"
