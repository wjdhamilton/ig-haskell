{-# LANGUAGE OverloadedStrings #-}
module IG.REST.LoginSpec (spec) where

import Data.Either
import Data.Either.Unwrap hiding (isRight)
import Data.Text as Text hiding (filter, head, map)
import IG
import IG.REST.Login
import Test.Hspec

spec :: Spec
spec = do
  describe "login" loginSpec
  describe "logout" logoutSpec
  describe "switchAccount" switchAccountSpec


loginSpec :: Spec
loginSpec = do
  context "with valid credentials" $ do
    (apiKey, loginDetails) <- runIO $ getCredentials True
    response <- runIO $ login True apiKey loginDetails

    it "should return Right" $ do
      isRight response `shouldBe` True


  context "with invalid credentials" $ do
    (apiKey, loginDetails) <- runIO getBadCredentials

    it "should throw an error with InvalidDetails" $ do
      login True apiKey loginDetails `shouldThrow` (errorCall . show $ InvalidDetails)


getBadCredentials :: IO (Text, LoginBody)
getBadCredentials = do
  (apiKey, _) <- getCredentials True
  let username = "evil"
  let password = "bad"
  let loginDetails = LoginBody False username password
  return $ (apiKey, loginDetails)


logoutSpec :: Spec
logoutSpec = do
  eh <- runIO $ loginToApi True
  let (headers, _) = fromRight eh
  it "should logout without errors" $ do
    logout headers `shouldReturn` Right ()


-- | Note: Requires a login with two different test accounts attached to it. 
switchAccountSpec :: Spec
switchAccountSpec = do
  eh <- runIO $ loginToApi True
  let (headers, loginResponse) = fromRight eh
  let currentAccount = currentAccountId loginResponse
  let otherAccounts = filter (\accId -> accId /= currentAccount) 
                    . map (\acc -> accountId acc) 
                    $ accounts loginResponse

  context "with valid account id submitted" $ do
    it "should succeed" $ do
      switchAccount headers (head otherAccounts) False `shouldReturn` Right ()
  

