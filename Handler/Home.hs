module Handler.Home where

import Handler.Ratings
import Import
import Forms

import Data.CountryCodes
import Data.Double.Conversion.Text
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
          ratings <- getRatings
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
      The Project uses something called "quadratic voting". The idea is to get
      more accurate expressions of your opinions by making you prioritize. For
      every program you rate, twenty-five points are added to your total rating
      budget. Giving something a rating costs as many points as the rating
      squared, so rating something one costs one point, rating something
      two costs four points and so on. Both positive and negative ratings work.
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
exampleRatings = [("terri.bl", -6), ("Tolerable Pro 3", 1), ("Shootymans 4", 5)]

ratingsBoxW :: Hidden -> Ratings -> Widget
ratingsBoxW hide Ratings{..} =
  let noRatings = null ratingsRatings
      ratingsList =
        (map addCostAndEditable $ if noRatings then exampleRatings else ratingsRatings)
        ++
        [("", 0, 0, True)] -- blank line for new entries
      addCostAndEditable (name, score) = (name, score, score ^ (2 :: Int), False)
      ptsSpent = if noRatings
                 then (sum $ map ((^(2::Int)) . snd) exampleRatings)
                 else ratingsPtsSpent
      totalBudget = if noRatings
                    then (length exampleRatings * 25)
                    else ratingsTotalBudget
      averageSpent =
        toFixed 2 ((fromIntegral ptsSpent :: Double) / fromIntegral (length ratingsList))
  in [whamlet|
<#ratingsBox :hidden hide:style="display: none">
  $if noRatings
    ^{explainBoxW}
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
      $forall (name, score, cost, editable) <- ratingsList
        <tr :not editable && noRatings:.example-data>
          <td>
            <button .btn-minus .btn-score>
          <td .program-name>
            <input class="program-name" type="text" value="#{name}" :not editable:readonly>
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
