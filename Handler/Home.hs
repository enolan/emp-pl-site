module Handler.Home where

import Import

import Data.CountryCodes
import Forms

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
  defaultLayout $ do
    setTitle "Home"
    $(widgetFile "homepage")

whatDo :: Widget
whatDo = do
  mauth <- handlerToWidget $ maybeAuth
  case mauth of
    Nothing ->
      [whamlet|
You need to <a href=@{AuthR LoginR}>log in via Google to participate.</a>
Don't worry, we'll keep your answers anonymous.|]
    Just userEnt -> do
      [whamlet|
Great, you're logged in as #{userEmail $ entityVal userEnt}. Not you?
<a href=@{AuthR LogoutR}>Log out.</a>|]
      mdemo <- handlerToWidget $ runDB $ getBy (UniqueAssociatedUser $ entityKey userEnt)
      case mdemo of
        Just demoEnt -> [whamlet|Time to tell us about programs!|]
        Nothing -> do
          (formW, encType) <- handlerToWidget $ generateFormPost $ userForm (entityKey userEnt)
          [whamlet|
We need some quick demographic information before we start:
<form id=demoForm>
  ^{formW}
  <input type=submit>|]


countryField :: Field Handler CountryCode
countryField = selectFieldList countryList

userForm :: UserId -> Form UserDemographics
userForm uid = renderDivs $ UserDemographics <$>
  pure uid <*>
  areq yearField "Year of birth" Nothing <*>
  areq textField "Gender" Nothing <*>
  areq countryField "Country of residence" Nothing <*>
  areq boolField "Are you a computer programmer?" Nothing
