module Import.NoFoundation
    ( module Import,
      setTitle
    ) where

import Model                 as Import
import Model.CountryCodes    as Import ()
import Settings              as Import
import Settings.StaticFiles  as Import

import ClassyPrelude.Yesod as Import hiding (Handler, setTitle)
import Yesod.Auth            as Import
import Yesod.Core.Types      as Import (loggerSet)
import qualified Yesod.Core.Widget(setTitle)
import Yesod.Default.Config2 as Import

setTitle :: MonadWidget m => Html -> m ()
setTitle t = Yesod.Core.Widget.setTitle $
  "Software Quality Causes Project - " `mappend` t
