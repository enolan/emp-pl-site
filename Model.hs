{-# LANGUAGE FlexibleInstances #-}

module Model where

import ClassyPrelude.Yesod
import Data.CountryCodes
import Database.Persist.Quasi

import Model.CountryCodes()

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

instance ToJSON Program
instance ToJSON Rating
