{-|
Module      : Move.Status
Description : Is move possible to do?
-}

module Move.Status where

import           Data.Aeson
import           GHC.Generics

data Status = OK | NothingToMove | OutOfBoard | DestinationNotEmpty | SheepCannotGoBack deriving (Enum, Eq, Generic, Ord, Read, Show)

instance FromJSON Status
instance ToJSON Status

