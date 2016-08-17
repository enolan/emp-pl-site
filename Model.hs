{-# LANGUAGE FlexibleInstances #-}

module Model where

import ClassyPrelude.Yesod
import Data.CountryCodes
import Database.Persist.Quasi

import Model.CountryCodes()

data RatingSort = HighestScore | LowestScore | RandomSort
  deriving (Eq, Read, Show)

instance PathPiece RatingSort where
  fromPathPiece = readMay
  toPathPiece = tshow

data HowMany = All | Some Int
  deriving (Eq, Read, Show)

instance PathPiece HowMany where
  fromPathPiece txt = case readMay txt of
    Just (i :: Int) -> Just $ Some i
    Nothing -> case readMay txt of
      Just All -> Just All
      _ -> Nothing
  toPathPiece = tshow

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")
