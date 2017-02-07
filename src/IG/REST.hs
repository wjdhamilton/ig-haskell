{-# LANGUAGE OverloadedStrings #-}

module IG.REST where

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString.Lazy as BL
import Data.Monoid
import Data.Text as Text
import Data.Text.Encoding as TE
import IG
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


-- | Takes an IO call to the API and returns an instance of Either containing
-- of Left ApiError or Right TheRequestedType
apiRequest :: (FromJSON a) => IO (Response BL.ByteString) -> IO (Either ApiError a)
apiRequest request = do
  response <- request
  let body = response ^. responseBody
  case response ^. responseStatus ^. statusCode of
       200 -> do
         case eitherDecode body of
              Left e -> return $ Left (UnknownError $ Text.pack e)
              Right r -> return $ Right r

       _ -> do return $ Left (decodeError body)
