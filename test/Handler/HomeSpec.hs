module Handler.HomeSpec (spec) where

import TestImport

import Data.Maybe (fromJust)
import System.IO.Unsafe (unsafePerformIO)
import qualified Test.Hspec.WebDriver as WD
import qualified Test.WebDriver.Commands.Wait as WDWait

mkUrl :: (IsString s, Semigroup s) => s -> s
mkUrl = ("https://localhost:3443" <>)

{-# NOINLINE settings #-}
settings :: AppSettings
settings = unsafePerformIO $
  loadYamlSettings
  ["config/test-settings.yml", "config/settings.yml"]
  []
  useEnv

isClickable :: WD.Element -> WD.WD Bool
isClickable el = (&&) <$> displayed <*> enabled
  where displayed = WD.isDisplayed el
        enabled = WD.isEnabled el

wait :: WD.WD a -> WD.WD a
wait = WDWait.waitUntil 5

spec :: Spec
spec = withServer $
  WD.session "login flow" $ WD.using WD.Firefox $ do
    it "loads the homepage" $ WD.runWD $
      WD.openPage $ mkUrl "/"
    it "lets you log in" $ WD.runWD $ do
      WD.openPage $ mkUrl "/"
      loginLink <- WD.findElem $ WD.ByPartialLinkText "log in via Google"
      loginHref <- WD.attr loginLink "href"
      loginHref `WD.shouldBe` Just (mkUrl "/auth/login")
      WD.click loginLink
      emailField <- WD.findElem $ WD.ById "Email"
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
      -- Just check it exists
      _demoForm <- WD.findElem $ WD.ById "demoForm"
      return ()
