{-# LANGUAGE OverloadedStrings #-}

module IG.REST where

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Data.Text
import Data.Text.Encoding as TE
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types hiding (statusCode)
import Network.Wreq

-- | Encapsulates the Client Security Token and Account Session access token. 
-- These are both required for accessing the api and are acquired using @login
data AuthHeaders = AuthHeaders {
                               -- | The Client Security Token
                               cst :: Text
                               -- | The account session security access token
                               , securityToken :: Text
                               , apiToken :: Text
                               , isDemo :: Bool
                               } deriving (Show)

buildHeaders :: Text -> AuthHeaders -> Options
buildHeaders version (AuthHeaders c x k _) = 
  baseHeaders version k & header "CST" .~ [TE.encodeUtf8 c]
                        & header "X-SECURITY-TOKEN" .~ [TE.encodeUtf8 x]


baseHeaders :: Text -> Text -> Options
baseHeaders v key = defaults & header "Accept" .~ ["application/json"]
                             & header "Content-Type" .~ [ "application/json" ]
                             & header "Version" .~ [ TE.encodeUtf8 v ]
                             & header "X-IG-API-KEY" .~ [TE.encodeUtf8 key]
                             & manager .~ Left (tlsManagerSettings)
                             & checkStatus .~ (Just $ \_ _ _  -> Nothing)
