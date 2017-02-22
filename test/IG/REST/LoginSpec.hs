{-# LANGUAGE OverloadedStrings #-}
module IG.REST.LoginSpec (spec) where

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Data.Either
import Data.Either.Unwrap hiding (isRight)
import Data.Maybe
import Data.Monoid
import Data.Text as Text hiding (filter, head, map)
import IG
import IG.REST.Login
import System.IO
import System.Directory
import Test.Hspec
import Tools

spec :: Spec
spec = do
  describe "login" loginSpec
  describe "logout" logoutSpec
  describe "switchAccount" switchAccountSpec


loginSpec :: Spec
loginSpec = do
  context "with valid credentials" $ do
    (apiKey, loginDetails) <- runIO getCredentials
    response <- runIO $ login True apiKey loginDetails

    it "should return Right" $ do
      isRight response `shouldBe` True


  context "with invalid credentials" $ do
    (apiKey, loginDetails) <- runIO getBadCredentials

    it "should throw an error with InvalidDetails" $ do
      login True apiKey loginDetails `shouldThrow` (errorCall . show $ InvalidDetails)


getBadCredentials :: IO (Text, LoginBody)
getBadCredentials = do
  creds <- readFile "test/config.json"
  let apiKey = getApiKey creds
  let username = "evil"
  let password = "bad"
  let loginDetails = LoginBody False username password
  return $ (apiKey, loginDetails)


logoutSpec :: Spec
logoutSpec = do
  (headers, _) <- loginToApi
  it "should logout without errors" $ do
    logout headers `shouldReturn` Right ()


-- | Note: Requires a login with two different test accounts attached to it. 
switchAccountSpec :: Spec
switchAccountSpec = do
  (headers, loginResponse) <- loginToApi
  let currentAccount = currentAccountId loginResponse
  let otherAccounts = filter (\accId -> accId /= currentAccount) 
                    . map (\acc -> accountId acc) 
                    $ accounts loginResponse

  context "with valid account id submitted" $ do
    it "should succeed" $ do
      switchAccount headers (head otherAccounts) False `shouldReturn` Right ()
  

