{-# OPTIONS_GHC -fno-warn-orphans #-}
module Model.CountryCodes where

import ClassyPrelude.Yesod
import Data.CountryCodes

derivePersistField "CountryCode"
