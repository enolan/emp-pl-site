{-# LANGUAGE DataKinds, TypeOperators #-}
module HostnameApi where

import Import

import Servant.API

type HostnameApi = "target" :> ReqBody '[PlainText] Text :> Post '[JSON] Bool
