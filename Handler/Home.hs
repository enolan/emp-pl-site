module Handler.Home where

import Handler.Ratings
import Import
import Forms

import Data.CountryCodes
import Yesod.Form.Bootstrap3

getHomeR :: Handler Html
getHomeR = do
  mauth <- maybeAuth
  whatDo <- case mauth of
        Nothing -> return pleaseLoginW
        Just userEnt -> do
          demoCount <- runDB $
            count [Filter UserDemographicsUser (Left $ entityKey userEnt) Eq]
          (formW, _) <- generateFormPost demoForm
          ratings <- if userGotExplanation $ entityVal userEnt
                then Just <$> getRatings
                else return Nothing
          return $ do
            loggedInW (entityVal userEnt)
            case demoCount of
              1 -> ratingsBoxW Visible ratings
              0 -> demoFormW formW >> ratingsBoxW Hidden ratings
              _ -> error
                "impossible: more than one UserDemographics for a given user"
  defaultLayout $ do
    setTitle "Home"
    $(widgetFile "homepage")

pleaseLoginW :: Widget
pleaseLoginW = [whamlet|
<p>
  You need to <a href=@{AuthR LoginR}>log in via Google to participate.</a>
  Don't worry, we'll keep your answers anonymous.|]

loggedInW :: User -> Widget
loggedInW u = [whamlet|
<p>
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
      For every program you rate, you get twenty-five points to spend however
      you like. Giving something a rating costs as many points as the rating
      squared, so rating something one costs one point, rating something two
      costs four points and so on.
    <p>
      Feel free to play with the entries below, then hit the button to dismiss
      this box and get started.
    <button #explanationButton .btn .btn-md .btn-block .btn-default>Got it</button>|]

data Hidden = Hidden | Visible
hidden :: Hidden -> Bool
hidden Hidden  = True
hidden Visible = False

data NeedsExplanation = NeedsExplanation | AlreadyExplained deriving Eq

exampleRatings :: [(Text, Int)]
exampleRatings = [("terri.bl", -7), ("Tolerable Pro 3", 1), ("Shootymans 4", 5)]

ratingsBoxW ::
  Hidden -> Maybe Ratings -> Widget
ratingsBoxW hide mbRatings =
  let ratingsList = map addCost $ maybe exampleRatings ratingsRatings mbRatings
      addCost (name, score) = (name, score, score ^ (2 :: Int))
      ptsSpent = maybe
        (sum $ map ((^(2::Int)) . snd) exampleRatings)
        ratingsPtsSpent
        mbRatings
      totalBudget = maybe
        (length exampleRatings * 25)
        ratingsTotalBudget
        mbRatings
      averageSpent =
        tshow ((fromIntegral ptsSpent :: Float) / fromIntegral (length ratingsList))
  in [whamlet|
<#ratingsBox :hidden hide:style="display: none">
  $case mbRatings
    $of Nothing
      ^{explainBoxW}
    $of Just _
  <table .table .table-bordered #points-table>
    <thead>
      <tr>
        <td>Total budget
        <td>Points remaining
        <td>Average spent per program
    <tbody>
      <tr>
        <td #total-budget>#{totalBudget}
        <td #available-points>#{totalBudget - ptsSpent}
        <td #average-points-spent>#{averageSpent}
  <table .table #ratings-table>
    <thead>
      <tr>
        <td .rating-btn-col>
        <td .program-name-col .program-name>Program name
        <td .rating-btn-col>
        <td .score-col>Score
        <td .cost-col>Cost
        <td .rating-btn-col>
    <tbody>
      $forall (name, score, cost) <- ratingsList
        <tr>
          <td>
            <button .btn-minus .btn-score>
          <td .program-name>
            <input class="program-name" type="text" value="#{name}" readonly>
          <td>
            <button .btn-plus .btn-score>
          <td>
            <span .score>#{score}
          <td>
            <span .cost>#{cost}
          <td>
            <button .btn-score .btn-delete>
|]

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
