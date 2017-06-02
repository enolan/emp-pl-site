module Handler.Privacy where

import Import

getPrivacyR :: Handler Html
getPrivacyR = defaultLayout $ do
  setTitle "Privacy policy"
  $(widgetFile "privacy")
