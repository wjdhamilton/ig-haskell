{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module IG where

import Control.Lens hiding ((|>))
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding
import Flow
import Network.Wreq
import Prelude hiding (lookup)
import Text.Regex.PCRE hiding (empty)

-- | Returns the host url of the IG API server. 
host :: Bool -- ^ If True then the demo address is returned, production otherwise
     -> Text -- ^ The host url
host isDemo = if isDemo then demoPath
                        else productionPath

productionPath :: Text
productionPath = "https://api.ig.com/"

demoPath :: Text
demoPath = "https://demo-api.ig.com/"

-- | Represents the different types of error response returned by the api. The
-- definitions are lifted directly from https://labs.ig.com/rest-trading-api-reference/service-detail?id=534
data ApiError = AccountDisabled -- ^ The user's preferred account is disabled.
              | AllowanceExceeded -- ^ The account traffic allowance has been exceeded
              | ApiKeyAllowanceExceeded -- ^ The api key traffic allowance has been exceeded
              | ApiKeyMissing -- ^ The api key was not provided
              | ApiKeyRevoked -- ^ The provided api key was not accepted because it has been revoked
              | ApiKeyUnaccepted -- ^ The provided api key was unaccepted. 
              | CannotUseApi -- ^ The account is not allowed to log into public API. Please use the web platform.
              | CredentialsMissing -- ^ The user has not provided all required security credentials
              | DealNotFound -- ^ The referenced deal could not be found
              | DisabledApiKey -- ^ The provided api key was not accepted because it is not currently enabled
              | EncryptionRequired -- ^ A login has been attempted to the login V1 service by a client from the IG Singapore company. They need to use the v2 version as they need to send their passwords encrypted.
              | HistoricalAllowanceExceeded -- ^ The account historical data traffic allowance has been exceeded
              | InvalidAccountToken -- ^  The service requires an account token and the one provided was not valid
              | InvalidApiKey -- ^ The provided api key was not valid for the requesting account
              | InvalidClientToken -- ^ The service requires a client token and the one provided was not valid
              | InvalidDateRange -- ^ Invalid date range
              | InvalidInput -- ^ A generic input data error has occurred
              | InvalidOAuthToken -- ^ Invalid OAuth access token (OAuth is not 
                                  -- used in the current version. This is for
                                  -- future compatibility)
              | MissingAccountToken -- ^ The service requires an account token and it was not provided
              | MissingClientToken -- ^ The service requires a client token and it was not provided
              | NeedPreferredAccount -- ^ The user has not set a preferred account.
              | SessionTimeout -- ^ Request timed out while retrieving session details
              | StockBrokingUnsupported -- ^ Stockbroking not supported for Public API users.
              | TradingAllowanceExceeded -- ^ The account trading traffic allowance has been exceeded
              | UnknownError Text -- ^ The error is not specified by the api
                deriving (Eq, Ord, Show)


apiError :: Response BL.ByteString -> Map ApiError (IO a)-> IO a
apiError resp responseMap  = 
  let errorCode = decodeError $ resp ^. responseBody in
    case lookup errorCode responseMap of
         Nothing -> do error $ show errorCode
         Just e -> e


decodeError :: BL.ByteString -> ApiError
decodeError err = case decode err >>= findError of
                     Nothing -> UnknownError (decodeUtf8 . BS.concat . BL.toChunks $ err)
                     Just e -> e


findError :: Value -> Maybe ApiError
findError (String s) = lookup s errorMap
findError (Object o) = lookup key errorMap
  where key = case HM.lookup "errorCode" o of
                   Nothing -> "oops"
                   Just (String e) -> e
                   -- errorCode should always map to a string
                   Just x          -> "oops"


errorMap :: Map Text ApiError
errorMap = empty
             |> insert "error.public-api.failure.encryption.required" EncryptionRequired
             |> insert "error.request.invalid.date-range" InvalidDateRange
             |> insert "error.security.api-key-missing" ApiKeyMissing
             |> insert "invalid.input" InvalidInput
             |> insert "error.public-api.failure.kyc.required" CannotUseApi
             |> insert "error.public-api.failure.missing.credentials" CredentialsMissing
             |> insert "error.public-api.failure.pending.agreements.required" CannotUseApi
             |> insert "error.public-api.failure.preferred.account.disabled" AccountDisabled
             |> insert "error.public-api.failure.preferred.account.not.set" NeedPreferredAccount
             |> insert "error.security.account-token-invalid" InvalidAccountToken
             |> insert "error.security.account-token-missing" MissingAccountToken
             |> insert "error.security.client-token-invalid" InvalidClientToken
             |> insert "error.security.client-token-missing" MissingClientToken
             |> insert "error.security.oauth-token-invalid" InvalidOAuthToken
             |> insert "endpoint.unavailable.for.api.key" ApiKeyUnaccepted
             |> insert "error.public-api.exceeded-account-allowance" AllowanceExceeded
             |> insert "error.public-api.exceeded-historical-data-allowance" HistoricalAllowanceExceeded
             |> insert "error.public-api.exceeded-trading-allowance" TradingAllowanceExceeded
             |> insert "error.public-api.exceeded-api-key-allowance" ApiKeyAllowanceExceeded
             |> insert "error.public-api.failure.stockbroking-not-supported" StockBrokingUnsupported
             |> insert "error.security.api-key-disabled" DisabledApiKey
             |> insert "error.security.api-key-invalid" InvalidApiKey
             |> insert "error.security.api-key-revoked" ApiKeyRevoked
             |> insert "error.security.get.session.timeout" SessionTimeout
             |> insert "error.confirms.deal-not-found" DealNotFound
