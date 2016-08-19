module Handler.Home where

import Handler.Ratings
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
          let explain = if userGotExplanation $ entityVal userEnt
                then AlreadyExplained
                else NeedsExplanation
          return $ do
            loggedInW (entityVal userEnt)
            case demoCount of
              1 -> ratingsBoxW Visible explain []
              0 -> demoFormW formW >> ratingsBoxW Hidden explain []
              _ -> error
                "impossible: more than one UserDemographics for a given user"
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
<form id=demoForm>
  We need some quick demographic information before we start:
  ^{innerForm}
  <.error-container>|]

explainBoxW :: Widget
explainBoxW = [whamlet|
  <#explainBox .container-fluid>
    <p>
      For every program you rate, you get four points to spend however you like.
      Giving something a rating costs as many points as the rating squared, so
      rating something one costs one point, rating something two costs four
      points and so on.
    <p>
      Feel free to play with the entries below, then hit the button to dismiss
      this box and get started.
    <button #explanationButton .btn .btn-md .btn-block .btn-default>Got it</button>|]

data Hidden = Hidden | Visible
hidden :: Hidden -> Bool
hidden Hidden  = True
hidden Visible = False

data NeedsExplanation = NeedsExplanation | AlreadyExplained deriving Eq

ratingsBoxW :: Hidden -> NeedsExplanation -> [Rating] -> Widget
ratingsBoxW hide explain _ = [whamlet|
<#ratingsBox :hidden hide:style="display: none">
  $if explain == NeedsExplanation
    ^{explainBoxW}
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

postDemoFormR :: Handler ()
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
            Right _ -> return ()
    _ -> invalidArgs []

postExplainedR :: Handler ()
postExplainedR = do
  auth <- requireAuthId
  runDB $ update auth [UserGotExplanation =. True]
