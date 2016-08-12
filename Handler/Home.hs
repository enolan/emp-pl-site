module Handler.Home where

import Import
import Forms

import Data.CountryCodes
import Yesod.Form.Bootstrap3

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
        Just _demoEnt -> [whamlet|Time to tell us about programs!|]
        Nothing -> do
          (formW, _encType) <- handlerToWidget $ generateFormPost userForm
          [whamlet|
We need some quick demographic information before we start:
<form id=demoForm>
  ^{formW}
  <.error-container>|]

countryField :: Field Handler CountryCode
countryField = selectFieldList countryList

userForm :: Form (UTCTime, Text, CountryCode, Bool)
userForm = renderBootstrap3 BootstrapBasicForm $ (,,,) <$>
  areq yearField (bfs' "Year of birth") Nothing <*>
  areq textField (bfs' "Gender") Nothing <*>
  areq countryField (bfs' "Country of residence") Nothing <*>
  areq bsBoolField (bfs' "Are you a computer programmer?") Nothing <*
  bootstrapSubmit ("Submit" :: BootstrapSubmit Text)

postDemoFormR :: Handler Value
postDemoFormR = do
  ((formData, _), _) <- runFormPost $ userForm
  mauth <- maybeAuth
  case formData of
    FormSuccess (birthYear, gender, residence, programmer) ->
      case mauth of
        Nothing -> permissionDenied "Login to add demographic information"
        Just uid -> do
          res <- runDB $ insertBy $
            UserDemographics {userDemographicsUser = entityKey uid
                            ,userDemographicsBirthYear = birthYear
                            ,userDemographicsGender = gender
                            ,userDemographicsResidence = residence
                            ,userDemographicsProgrammer = programmer}
          case res of
            Left _ -> invalidArgs ["User already has demographics"]
            Right _ -> return $ toJSON $ PostDemoResponse True
    _ -> invalidArgs []

data PostDemoResponse = PostDemoResponse {ok :: Bool}
  deriving (Generic, Show)
instance ToJSON PostDemoResponse
instance FromJSON PostDemoResponse
