-- Junk drawer for slow to compile TH stuff
module Forms where

import Import

import Data.Time.Format (parseTimeM)
import Yesod.Form.Bootstrap3

yearField :: Field Handler UTCTime
yearField = Field
  { fieldParse = \vals _ -> case vals of
      [t] -> return $ case parseTimeM True defaultTimeLocale "%Y" (unpack t) of
        Left msg -> Left $ fromString msg
        Right time -> return $ Just time
      [] -> return $ Right Nothing
      _  -> return $ Left "too many years"
  , fieldView = \idAttr nameAttr otherAttrs _eResult req ->
     [whamlet|<input id=#{idAttr} name=#{nameAttr} *{otherAttrs} :req:required type=text>|]
  , fieldEnctype = UrlEncoded}

bfs' :: Text -> FieldSettings App
bfs' = bfs

bsBoolField :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m Bool
bsBoolField = boolField {fieldView =
  \_theId name _attrs val req -> [whamlet|
$newline never
<span .bool-container>
  <label .radio-inline>
    <input type=radio name=#{name} value=yes :showVal id val:checked :req:required>
    _{MsgBoolYes}
  <label .radio-inline>
    <input type=radio name=#{name} value=no :showVal not val:checked :req:required>
    _{MsgBoolNo}|]}
  where showVal = either (\_ -> False)
