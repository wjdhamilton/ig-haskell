{-# LANGUAGE OverloadedStrings #-}
module Tools where

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Data.Either.Unwrap
import Data.Maybe
import Data.Text
import IG
import IG.REST
import IG.REST.Login
import Test.Hspec


loginToApi = do
    (apiKey, loginDetails) <- runIO Tools.getCredentials
    response <- runIO $ login True apiKey loginDetails
    return $ fromRight response


getCredentials :: IO (Text, LoginBody)
getCredentials = do
  creds <- readFile "test/config.json"
  let apiKey = Tools.getApiKey creds
  let identifier = fromJust $ creds ^? key "username" . _String
  let password = fromJust $ creds ^? key "password" . _String
  let loginDetails = LoginBody False identifier password
  return $ (apiKey, loginDetails)


getApiKey :: String -> Text
getApiKey creds = fromJust $ creds ^? key "apiKey" . _String
