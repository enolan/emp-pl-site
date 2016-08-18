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
  mauth <- maybeAuth
  whatDo <- case mauth of
        Nothing -> return pleaseLoginW
        Just userEnt -> do
          demoCount <- runDB $
            count [Filter UserDemographicsUser (Left $ entityKey userEnt) Eq]
          (formW, _) <- generateFormPost demoForm
          return $ do
            loggedInW (entityVal userEnt)
            case demoCount of
              1 -> ratingsBoxW False
              0 -> demoFormW formW >> ratingsBoxW True
              _ -> error "impossible: more than one UserDemographics for a given user"
  defaultLayout $ do
    setTitle "Home"
    $(widgetFile "homepage")

pleaseLoginW :: Widget
pleaseLoginW = [whamlet|
  You need to <a href=@{AuthR LoginR}>log in via Google to participate.</a>
  Don't worry, we'll keep your answers anonymous.|]

loggedInW :: User -> Widget
loggedInW u = [whamlet|
  Great, you're logged in as #{userEmail u}. Not you?
  <a href=@{AuthR LogoutR}>Log out.</a>|]

demoFormW :: Widget -> Widget
demoFormW innerForm = [whamlet|
We need some quick demographic information before we start:
<form id=demoForm>
  ^{innerForm}
  <.error-container>|]

ratingsBoxW :: Bool -> Widget
ratingsBoxW hidden = [whamlet|
<#ratingsBox :hidden:style="display: none">
  SUP?|]

countryField :: Field Handler CountryCode
countryField = selectFieldList countryList

demoForm :: Form (UTCTime, Text, CountryCode, Bool)
demoForm = renderBootstrap3 BootstrapBasicForm $ (,,,) <$>
  areq yearField (bfs' "Year of birth") Nothing <*>
  areq textField (bfs' "Gender") Nothing <*>
  areq countryField (bfs' "Country of residence") Nothing <*>
  areq bsBoolField (bfs' "Are you a computer programmer?") Nothing <*
  bootstrapSubmit ("Submit" :: BootstrapSubmit Text)

postDemoFormR :: Handler Value
postDemoFormR = do
  ((formData, _), _) <- runFormPost $ demoForm
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
