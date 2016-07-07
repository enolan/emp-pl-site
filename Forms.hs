-- Junk drawer for slow to compile TH stuff
module Forms where

import Import

import Data.Time.Format (parseTimeM)

yearField :: Field Handler UTCTime
yearField = Field
  { fieldParse = \vals _ -> case vals of
      [t] -> return $ case parseTimeM True defaultTimeLocale "%Y" (unpack t) of
        Left msg -> Left $ fromString msg
        Right time -> return $ Just time
      [] -> return $ Right Nothing
      _  -> return $ Left "too many years"
  , fieldView = \idAttr nameAttr otherAttrs _eResult _req ->
     [whamlet|<input id=#{idAttr} name=#{nameAttr} *{otherAttrs} type=text>|]
  , fieldEnctype = UrlEncoded}
