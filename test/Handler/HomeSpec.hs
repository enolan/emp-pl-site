module Handler.HomeSpec (spec) where

import TestImport

import qualified Test.Hspec.WebDriver as WD

mkUrl :: String -> String
mkUrl = ("https://localhost:3443" <>)

spec :: Spec
spec = withServer $
  WD.session "login flow" $ WD.using WD.Firefox $ do
    it "loads the homepage" $ WD.runWD $
      WD.openPage $ mkUrl "/"
    it "asks you to log in" $ WD.runWD $ do
      WD.openPage $ mkUrl "/"
      liftIO $ threadDelay 5000000
      void $ WD.findElem $ WD.ByPartialLinkText "log in via Google"
