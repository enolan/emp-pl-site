module Handler.PrivacySpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ describe "privacy policy" $
  it "gives a 200" $ do
  get PrivacyR
  statusIs 200
