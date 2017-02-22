{-# LANGUAGE OverloadedStrings #-}
module IGSpec (spec) where

import Data.ByteString.Lazy.Char8 (ByteString) 
import Data.Map.Strict as Map
import qualified Data.ByteString.Lazy.Char8 as BL hiding (concat, map)
import Data.Monoid
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TE
import Test.Hspec
import Test.Hspec.Core.Spec hiding (describe, it)
import IG
import Network.HTTP.Client hiding (host, (<>))
import Network.HTTP.Client.Internal hiding (host, (<>))
import Network.HTTP.Types
import Network.HTTP.Types.Status

spec :: Spec
spec = do
  describe "host" hostSpec
  describe "apiError" apiErrorSpec
  describe "decodeError" decodeErrorSpec

hostSpec :: Spec
hostSpec = do
  it "returns the production address if isDemo = False" $ do
    host False `shouldBe` productionPath

  it "returns the demo address if isDemo = True" $ do
    host True `shouldBe` demoPath


apiErrorSpec :: Spec
apiErrorSpec = do
  it "Should raise an error if no error handlers are supplied" $ do
    let errorMessage = "Unknown"
    let response = mockResponse . TE.encodeUtf8 . TL.fromStrict $ errorMessage
    apiError response Map.empty `shouldThrow` (errorCall . show $ UnknownError errorMessage)

  it "Should run the error handler if an error handler is supplied" $ do
    let errorHandler = return True
    let response = mockResponse . TE.encodeUtf8 . TL.fromStrict $ "Unknown"
    let handlerMap = Map.singleton (UnknownError "Unknown") errorHandler
    result <- apiError response handlerMap 
    result `shouldBe` True


mockResponse :: BL.ByteString -> Response BL.ByteString
mockResponse err = Response
  { responseStatus = ok200
  , responseVersion = http11
  , responseHeaders = []
  , responseBody = err
  , responseCookieJar = createCookieJar []
  , responseClose' = ResponseClose (return () :: IO ())
  }


decodeErrorSpec = mapM_ toErrorSpec errors
  where toErrorSpec (key,val) = it (BL.unpack $ "detects " <> key) $ 
                                  decodeError (errorBody key) `shouldBe` val
        errors = [ ("error.public-api.failure.encryption.required", EncryptionRequired)
                 , ("error.request.invalid.date-range",  InvalidDateRange)
                 , ("error.security.api-key-missing",  ApiKeyMissing)
                 , ("invalid.input",  InvalidInput)
                 , ("error.public-api.failure.kyc.required",  CannotUseApi)
                 , ("error.public-api.failure.missing.credentials",  CredentialsMissing)
                 , ("error.public-api.failure.pending.agreements.required",  CannotUseApi)
                 , ("error.public-api.failure.preferred.account.disabled",  AccountDisabled)
                 , ("error.public-api.failure.preferred.account.not.set",  NeedPreferredAccount)
                 , ("error.security.account-token-invalid",  InvalidAccountToken)
                 , ("error.security.account-token-missing",  MissingAccountToken)
                 , ("error.security.client-token-invalid",  InvalidClientToken)
                 , ("error.security.client-token-missing",  MissingClientToken)
                 , ("error.security.oauth-token-invalid",  InvalidOAuthToken)
                 , ("endpoint.unavailable.for.api.key",  ApiKeyUnaccepted)
                 , ("error.public-api.exceeded-account-allowance",  AllowanceExceeded)
                 , ("error.public-api.exceeded-historical-data-allowance",  HistoricalAllowanceExceeded)
                 , ("error.public-api.exceeded-trading-allowance",  TradingAllowanceExceeded)
                 , ("error.public-api.exceeded-api-key-allowance",  ApiKeyAllowanceExceeded)
                 , ("error.public-api.failure.stockbroking-not-supported",  StockBrokingUnsupported)
                 , ("error.security.api-key-disabled",  DisabledApiKey)
                 , ("error.security.api-key-invalid",  InvalidApiKey)
                 , ("error.security.api-key-revoked",  ApiKeyRevoked)
                 , ("error.security.get.session.timeout",  SessionTimeout)
                 , ("error.confirms.deal-not-found",  DealNotFound)
                 , ("Unknown error", UnknownError . errorBody $ "Unknown error")
                 , ("validation.null-not-allowed.request.currencyCode", CurrencyCodeRequired)
                 ]


errorBody code = "{ \"errorCode\": \"" <> code <> "\"}"
