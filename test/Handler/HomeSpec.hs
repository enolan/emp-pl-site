{-# LANGUAGE ScopedTypeVariables #-}
module Handler.HomeSpec (spec) where

import           TestImport

import           Data.Aeson (Value)
import           Data.Maybe (fromJust)
import           Data.Text.ICU.Normalize
import           Database.Esqueleto hiding (Value, (==.), count, get)
import qualified Database.Esqueleto as E
import qualified Test.WebDriver as WD
import qualified Test.WebDriver.Class as WD
import qualified Test.WebDriver.Commands.Wait as WDWait
import           Yesod.Core.Handler (RedirectUrl, toTextUrl)

import Handler.Ratings (getPointsDB)

mkUrl :: (MonadReader (TestApp App) m, MonadIO m, RedirectUrl App r) =>
  r -> m Text
mkUrl r = ask >>= \(a, _) -> liftIO $ unsafeHandler a $ toTextUrl r

openRoute ::
  (MonadReader (TestApp App) m, MonadIO m,
   RedirectUrl App a, WD.WebDriver m) => a -> m ()
openRoute r = (unpack <$> mkUrl r) >>= WD.openPage

isClickable :: WD.WebDriver m => WD.Element -> m Bool
isClickable el = (&&) <$> displayed <*> enabled
  where displayed = WD.isDisplayed el
        enabled = WD.isEnabled el

wait :: WD.WebDriver m => m a -> m a
wait = WDWait.waitUntil 5

findFieldByLabel :: (WD.WebDriver m, MonadIO m) => Text -> m WD.Element
findFieldByLabel str = do
  labels <- WD.findElems $ WD.ByXPath $
    "//label[text() = \"" <> str <> "\"]"
  els <- catMaybes <$> mapM (`WD.attr` "for") labels
  liftIO $ els `shouldSatisfy` ((==1) . length)
  WD.findElem $ WD.ById $ headEx els

fillFieldByLabel :: (WD.WebDriver m, MonadIO m) => Text -> Text -> m ()
fillFieldByLabel label text = findFieldByLabel label >>= WD.sendKeys text

selectDropdownByLabel :: (WD.WebDriver m, MonadIO m) => Text -> Text -> m ()
selectDropdownByLabel label optionName = do
  field <- findFieldByLabel label
  targetOptions <- WD.findElemsFrom field $ WD.ByXPath $
    "option[text() = \"" <> optionName <> "\"]"
  liftIO $ targetOptions `shouldSatisfy` ((==1) . length)
  WD.click $ headEx targetOptions

selectBoolByLabel :: (WD.WebDriver m, MonadIO m) => Text -> Bool -> m ()
selectBoolByLabel labelText value = do
  labels <- filterM (\el -> (==labelText) <$> WD.getText el) =<<
    WD.findElems (WD.ByTag "label")
  liftIO $ labels `shouldSatisfy` ((==1) . length)
  formGroup <- WD.findElemFrom (headEx labels) $ WD.ByXPath ".."
  let target = if value then "yes" else "no"
  button <- WD.findElemFrom formGroup $
    WD.ByCSS $ "input[value=" <> target <> "]"
  WD.click button

assertDoesNotExist :: (WD.WebDriver m, MonadIO m) => WD.Selector -> m ()
assertDoesNotExist selector = do
  els <- WD.findElems selector
  liftIO $ length els `shouldBe` 0

spec :: Spec
spec = do
  withApp $ describe "homepage (yesod-test)" $
    it "loads the homepage" $
      get HomeR
  describe "homepage (selenium)" $ do
    it "lets you log in" $ withServerM $ do
      loginGoogle
      -- Just check it exists
      _demoForm <- wait $ WD.findElem $ WD.ById "demoForm"
      return ()
    it "lets you enter demographic information" $ withServerM $ do
      loginDummy
      enterDemo
      res <- runDB' $ count ([] :: [Filter UserDemographics])
      liftIO $ res `shouldBe` 1
    it "lets you dismiss the explanation" $ withServerM $ do
      loginDummy
      enterDemo
      let explainBox = WD.ById "explainBox"
          readOnlyPrograms = WD.ByCSS "input.program-name[readonly]"
          blankProgram = WD.ByCSS "input.program-name:not([readonly])"
          explanationButton = WD.ById "explanationButton"
      explanationBox <- WD.findElem explainBox
      examplePrograms <- WD.findElems readOnlyPrograms
      liftIO $ length examplePrograms `shouldBe` 3
      blankPrograms <- WD.findElems blankProgram
      liftIO $ length blankPrograms `shouldBe` 1
      WD.findElem explanationButton >>= WD.click
      wait $ WDWait.expect =<< WD.isDisplayed explanationBox
      assertDoesNotExist readOnlyPrograms
    it "maintains invariants when entering ratings" $ wdProperty $ do
      run $ loginDummy >> enterDemo
      ratingProp

ratingProp :: PropertyM (ReaderT (TestApp App) WD.WD) ()
ratingProp = do
  run $ WD.findElem (WD.ByCSS "button#explanationButton") >>= WD.click
  pick mkActs >>= go
  where
  go []       = validate
  go (a : as) = do
    run $ case a of
      Delete n -> do
        tr <- getTr n
        delBtn <- WD.findElemFrom tr $ WD.ByCSS ".btn-delete"
        WD.click delBtn
      AddProgram str -> do
        blankField <- WD.findElem $ WD.ByCSS "input.program-name:not([readonly])"
        WD.sendKeys (normalize NFC (pack str) <> "\n") blankField
      IncreaseRating n -> do
        tr <- getTr n
        incBtn <- WD.findElemFrom tr $ WD.ByCSS ".btn-plus"
        WD.click incBtn
      DecreaseRating n -> do
        tr <- getTr n
        decBtn <- WD.findElemFrom tr $ WD.ByCSS ".btn-minus"
        WD.click decBtn
    let waitForServer = do
          pendingTrs <- WD.findElems $ WD.ByCSS "tr.pending"
          WDWait.expect $ null pendingTrs
    run $ wait waitForServer
    validate
    go as
  getTr n = wait $ WD.findElem $ WD.ByXPath $
    "//table[@id='ratings-table']/tbody/tr[" <> tshow (n + 1) <> "]"
  validate = do
    trs <- run $ WD.findElems $ WD.ByXPath
      "//table[@id='ratings-table']/tbody/tr[td/input/@readonly]"
    tableTuple@(_tblPointsSpent, _tblTotalBudget) :: (Int, Int) <- foldM
      (\(pointsSpentSoFar, totalBudgetSoFar) tr -> do
          mbScore <- readMay <$> run
            (WD.findElemFrom tr (WD.ByCSS "span.score") >>= WD.getText)
          mbCost  <- readMay <$> run
            (WD.findElemFrom tr (WD.ByCSS "span.cost") >>= WD.getText)
          mbName <- run $ do
            input <- WD.findElemFrom tr (WD.ByCSS "input.program-name")
            WD.attr input "value"
          case (mbScore, mbCost, mbName) of
            (Just score, Just cost, Just name) -> if cost == score ^ (2 :: Int)
              then do
                dbScore <- run $ runDB' $ select $ from $
                  \(rating `InnerJoin` program) -> do
                    E.on (rating ^. RatingProgram E.==. program ^. ProgramId)
                    where_ (program ^. ProgramName E.==. val name)
                    return $ rating ^. RatingScore
                case dbScore of
                  [E.Value dbScore'] -> if dbScore' == score
                    then return
                      (pointsSpentSoFar + score ^ (2 :: Int),
                       totalBudgetSoFar + 25)
                    else fail "db score not equal to dom score"
                  _                  -> fail "dbScore not exactly one element"
              else fail "cost not equal to score^2"
            _                       -> fail "score or cost didn't parse")
      (0, 0)
      trs
    (totalBudgetSummary, pointsRemainingSummary) <- do
      mbTotalBudget <- readMay <$> run
        (WD.findElem (WD.ByCSS "#total-budget") >>= WD.getText)
      mbPointsRemaining <- readMay <$> run
        (WD.findElem (WD.ByCSS "#available-points") >>= WD.getText)
      case (mbTotalBudget, mbPointsRemaining) of
        (Just totalBudget, Just pointsRemaining) ->
          return (totalBudget, pointsRemaining)
        _ -> fail "couldn't parse summary table"
    unless
      ((totalBudgetSummary - pointsRemainingSummary, totalBudgetSummary)
       ==
       tableTuple) $
      fail "summary table values not equal to summary computed from table"
    uid <- fmap entityKey <$> run (runDB' $ getBy (UniqueEmail "test"))
    case uid of
      Just uid' -> do
        dbTuple@(_pointsSpentDB, _totalBudgetDB) <-
          run $ runDB' $ getPointsDB uid'
        unless (dbTuple == tableTuple) $ fail "dbTuple != domTuple"
      Nothing   -> fail "couldn't find test user in db"

mkActs :: Gen [RatingAction]
mkActs = sized (\s -> choose (0,s) >>= go 0)
  where
  go _ 0    = return []
  go 0 size = do addpr <- AddProgram <$> arbitrary
                 rest <- go' 1 (size - 1)
                 return $ addpr:rest
  go n size = frequency
    [(1, do n' <- choose (0, n - 1)
            rest <- go' (n - 1) (size - 1)
            return $ Delete n' : rest),
     (1, do addpr <- AddProgram <$> arbitrary
            rest <- go' (n + 1) (size - 1)
            return $ addpr:rest),
     (2, do n' <- choose (0, n - 1)
            rest <- go' n (size - 1)
            action <- elements $ map ($ n') [IncreaseRating, DecreaseRating]
            return $ action : rest)
    ]
  go' n size = resize size $ go n size

data RatingAction = Delete Int
                  | AddProgram String
                  | IncreaseRating Int
                  | DecreaseRating Int deriving (Show)

loginDummy :: (MonadReader (TestApp App) m, WD.WebDriver m, MonadIO m) => m ()
loginDummy = do
      openRoute HomeR
      _ :: Value <- WD.executeJS [] $
        "$.ajax({async: false, data: {ident: \"test\"}, method: \"POST\"," <>
        "url: \"https://localhost:3443/auth/page/dummy\"})"
      openRoute HomeR

loginGoogle :: (MonadReader (TestApp App) m, WD.WebDriver m, MonadIO m) => m ()
loginGoogle = do
      openRoute HomeR
      loginLink <- WD.findElem $ WD.ByPartialLinkText "log in via Google"
      loginHref <- WD.attr loginLink "href"
      authUrl <- mkUrl $ AuthR LoginR
      liftIO $ loginHref `shouldBe` Just authUrl
      WD.click loginLink
      emailField <- WD.findElem $ WD.ById "Email"
      settings <- (appSettings . fst) <$> ask
      WD.sendKeys (fromJust $ googleUser settings) emailField
      nextButton <- WD.findElem $ WD.ById "next"
      WD.click nextButton
      passwdField <- wait $ WD.findElem $ WD.ById "Passwd"
      WD.sendKeys (fromJust $ googlePassword settings) passwdField
      signInButton <- WD.findElem $ WD.ById "signIn"
      WD.click signInButton
      approveButton <- WD.findElem $ WD.ById "submit_approve_access"
      wait $ WDWait.expect =<< isClickable approveButton
      WD.click approveButton

enterDemo :: (MonadReader (TestApp App) m, WD.WebDriver m, MonadIO m) => m ()
enterDemo = do
    fillFieldByLabel "Year of birth" "1990"
    fillFieldByLabel "Gender" "male"
    selectDropdownByLabel "Country of residence" "United States"
    selectBoolByLabel "Are you a computer programmer?" True
    WD.findElem (WD.ByTag "form") >>= WD.submit
