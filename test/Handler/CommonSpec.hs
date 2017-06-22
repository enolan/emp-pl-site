module Handler.CommonSpec (spec) where

import TestImport

import Network.HTTP.Types.Header

spec :: Spec
spec = withApp $ do
    describe "robots.txt" $ do
        it "gives a 200" $ do
            get RobotsR
            statusIs 200
        it "has correct User-agent" $ do
            get RobotsR
            bodyContains "User-agent: *"
    describe "favicon.ico" $ do
        it "gives a 200" $ do
            get FaviconR
            statusIs 200
    describe "referer tracking" $ do
      it "puts the original referer in the user record" $ do
        request $ do
          setUrl $ HomeR
          addRequestHeader (hReferer, "http://referertest.com")
        request $ do
          setMethod "POST"
          addPostParam "ident" "test"
          setUrl $ AuthR $ PluginR "dummy" []
          addTokenFromCookie
        res <- (fmap (userOrigReferer . entityVal)) <$> (runDB $ getBy $ UniqueEmail "test")
        assertEq "userOrigReferer" res (Just $ Just "http://referertest.com")
