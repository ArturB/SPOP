{-|
Module      : Board.Field
Description : Board field. 
Datatype that represents the board field. A field may be empty, or may be occupied by sheep or by a wolf. 
-}

module Board.Field where

import           Data.Aeson
import           GHC.Generics

-- | Datatype that represents the board field. A field may be empty, or may be occupied by sheep or by a wolf. 
data Field = Empty | Sheep | Wolf deriving (Enum, Eq, Ord, Generic)

instance FromJSON Field
instance ToJSON Field 

-- | Print a wolf as 'w' character, sheep as 'o' character and empty field as a dot. 
instance Show Field where
    show Empty = "."
    show Sheep = "o"
    show Wolf  = "w"
