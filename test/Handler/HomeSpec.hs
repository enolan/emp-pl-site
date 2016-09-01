{-# LANGUAGE ScopedTypeVariables #-}
module Handler.HomeSpec (spec) where

import           TestImport

import           Data.Maybe (fromJust)
import qualified Test.WebDriver as WD
import qualified Test.WebDriver.Class as WD
import qualified Test.WebDriver.Commands.Wait as WDWait
import           Yesod.Core.Handler (RedirectUrl, toTextUrl)

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
  labels :: [WD.Element] <- filterM (\el -> (==str) <$> WD.getText el) =<<
    WD.findElems (WD.ByTag "label")
  els <- catMaybes <$> mapM (`WD.attr` "for") labels
  liftIO $ els `shouldSatisfy` ((==1) . length)
  WD.findElem $ WD.ById $ headEx els

fillFieldByLabel :: (WD.WebDriver m, MonadIO m) => Text -> Text -> m ()
fillFieldByLabel label text = findFieldByLabel label >>= WD.sendKeys text

findChildrenByLabel :: (WD.WebDriver m, MonadIO m) =>
  Text -> Text -> m [WD.Element]
findChildrenByLabel label tagName = do
  parentEl <- findFieldByLabel label
  WD.findElemsFrom parentEl $ WD.ByTag tagName

findChildByLabelF :: (WD.WebDriver m, MonadIO m) =>
  Text -> Text -> (WD.Element -> m Bool) -> m WD.Element
findChildByLabelF label tagName f = do
  children <- findChildrenByLabel label tagName
  child <- filterM f children
  liftIO $ child `shouldSatisfy` ((==1) . length)
  return (headEx child)

selectDropdownByLabel :: (WD.WebDriver m, MonadIO m) => Text -> Text -> m ()
selectDropdownByLabel label optionName = do
  targetOption <- findChildByLabelF
    label "option" (\el -> (== optionName) <$> WD.getText el)
  WD.click targetOption

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

spec :: Spec
spec = describe "homepage" $ do
  it "loads the homepage" $ withServerM $
    openRoute HomeR
  it "lets you log in" $ withServerM $ do
      login
      -- Just check it exists
      _demoForm <- WD.findElem $ WD.ById "demoForm"
      return ()
  it "lets you enter demographic information" $ withServerM $ do
    login
    fillFieldByLabel "Year of birth" "1990"
    fillFieldByLabel "Gender" "male"
    selectDropdownByLabel "Country of residence" "United States"
    selectBoolByLabel "Are you a computer programmer?" True
    WD.findElem (WD.ByTag "form") >>= WD.submit
    res <- runDB' $ count ([] :: [Filter UserDemographics])
    liftIO $ res `shouldBe` 1

login :: (MonadReader (TestApp App) m, WD.WebDriver m, MonadIO m) => m ()
login = do
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
