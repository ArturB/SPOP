{-|
Module      : Move.Status
Description : Is a move valid?
Some moves are forbidden for some reasons. 
-}

module Move.Status where

import           Data.Aeson
import           GHC.Generics

-- | Possible move statuses. 
data Status = OK |                  -- ^ Move is valid and can be executed. 
              NothingToMove |       -- ^ Move cannot be done, because on specified position is no wolf or sheep to move. 
              OutOfBoard |          -- ^ Move cannot be done, because it will result in getting out the board. 
              DestinationNotEmpty | -- ^ Move cannot be done, because destination is occupied by another sheep or wolf.
              SheepCannotGoBack     -- ^ Move cannot be done, because sheeps can only move upwards. 
              deriving (Enum, Eq, Generic, Ord, Read, Show)

instance FromJSON Status
instance ToJSON Status

