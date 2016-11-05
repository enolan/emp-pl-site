-- Google requires a static hostname that ends in a valid TLD for OAuth, so
-- Selenium needs to see that. Setting it for stack exec in docker run-args
-- works but means more than one stack process can't run at the same time -
-- they'd have the same hostname and that's not allowed. This daemon runs on the
-- Selenium server and sets a line in /etc/hosts in response to requests from
-- the test runner.

module Main where

import ClassyPrelude

import HostnameApi
import Network.Wai.Handler.Warp
import Servant

main :: IO ()
main = do
  origHosts <- readFile "/etc/hosts"
  run 31337 $ serve (Proxy :: Proxy HostnameApi) $ server origHosts

server :: Text -> Server HostnameApi
server origHosts targetIP = do
  writeFile "/etc/hosts" $ origHosts ++ "\n" ++ targetIP ++ " stack-exec.org\n"
  return True
