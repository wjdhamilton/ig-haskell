{-# LANGUAGE DeriveGeneric #-}
-- | Functions for working with the Login Endpoint.

module IG.REST.Login where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Map.Strict as Map
import Data.Monoid
import Data.String.Conversions
import Data.Text as Text
import Data.Text.Encoding as TE
import Data.Scientific
import GHC.Generics
import IG (ApiError, apiError, decodeError, host)
import IG.REST
import Network.HTTP.Types hiding (statusCode)
import Network.Wreq
import Data.Either.Unwrap
import Data.Maybe


data Credentials = Credentials { demo :: Credential
                               , production :: Credential
                               } deriving (Generic, Show)


instance FromJSON Credentials


data Credential = Credential { apiKey :: Text
                  , username :: Text
                  , pass :: Text
                  } deriving (Generic, Show)

instance FromJSON Credential


loginToApi :: Bool -> IO (Either String (AuthHeaders, LoginResponse))
loginToApi isDemo = do
    (apiKey, loginDetails) <- getCredentials isDemo
    response <- login isDemo apiKey loginDetails
    return $ response


getCredentials :: Bool -> IO (Text, LoginBody)
getCredentials isDemo = do
  file <- readFile "ig_creds.json"
  let allCreds = fromJust . decode $ cs file
  let creds = if isDemo then demo allCreds
                        else production allCreds
  let a = apiKey creds
  let identifier = username creds
  let p = pass creds
  let loginDetails = LoginBody False identifier p
  return $ (a, loginDetails)


getKey :: String -> Text -> Text
getKey creds k = fromJust $ creds ^? key k . _String

-- | Represents the payload returned by the API when the user successfully logs
-- in.
data LoginResponse = LoginResponse { 
                                   -- | AccountInfo object
                                     accountInfo :: AccountInfo
                                   -- | Account type
                                   , accountType :: AccountType
                                   -- | Array of details for each user account
                                   , accounts :: [AccountDetails]
                                   -- | Client Identifier
                                   , clientId :: Text
                                   -- | Id of account in use
                                   , currentAccountId :: Text
                                   -- | Account Currency
                                   , currencyIsoCode :: Text
                                   -- | Account currency symbol
                                   , currencySymbol :: Text
                                   -- | Whether or not the account can deal
                                   , dealingEnabled :: Bool
                                   -- | True if the client has active demo accounts
                                   , hasActiveDemoAccounts :: Bool
                                   -- | True if the client has active live accounts
                                   , hasActiveLiveAccounts :: Bool
                                   -- | The endpoint for subscribing to Lightstreamer
                                   , lightstreamerEndpoint :: Text
                                   -- | Describes the environment to be used as the rerouting destination
                                   , reroutingEnvironment :: Maybe ReroutingEnvironment
                                   -- | Client account timezone offset relative to UTC, expressed in hours
                                   , timezoneOffset :: Integer
                                   -- | True if client can use trailing stops
                                   , trailingStopsEnabled :: Bool
                                   } deriving (Eq, Generic, Show)

instance FromJSON LoginResponse

                                  
-- | Client Account Info as returned by a login request
data AccountInfo = AccountInfo {
                               -- | Account funds available for trading
                                 available :: Scientific
                               -- | Balance of funds in the account
                               , balance :: Scientific
                               -- | Minimum deposit required for margins
                               , deposit :: Scientific
                               -- | Account profit and loss
                               , profitLoss :: Scientific
                               } deriving (Eq, Generic, Show)


instance FromJSON AccountInfo

-- | The three different types of account that IG support
data AccountType = CFD
                 | PHYSICAL
                 | SPREADBET
                 deriving (Eq, Generic, Show)
                                  

instance FromJSON AccountType


-- | The Details of an account
data AccountDetails = AccountDetails {
                                     -- | The account id
                                       accountId :: Text
                                     -- | The Account name
                                     , accountName :: Text
                                     -- | The type of account
                                     , accountType' :: AccountType
                                     -- | Indicates where or not this is the preferred account
                                     , preferred :: Bool
                                     } deriving (Eq, Generic, Show)

instance FromJSON AccountDetails where

  parseJSON = withObject "accountDetails" $ \o -> do
    id <- o .: "accountId"
    name <- o .: "accountName"
    type' <- o .: "accountType"
    pref <- o .: "preferred"
    return $ AccountDetails id name type' pref


-- | TODO: What is the Rerouting Environment?
data ReroutingEnvironment = DEMO
                          | LIVE
                          | TEST
                          | UAT
                          deriving (Eq, Generic, Show)

instance FromJSON ReroutingEnvironment

-- | Represents the body content required to successfully log into the api
data LoginBody = LoginBody { 
                           -- | Whether or not the password has been encrypted
                             encryptedPassword :: Bool
                           -- | Client login identifier (username)
                           , identifier :: Text
                           -- | The client password
                           , password :: Text
                           } deriving (Eq, Generic, Show)

instance ToJSON LoginBody

restPath :: Text
restPath = "gateway/deal/session"

-- | Log in to the IG API. On success, a 
login :: Bool -> Text -> LoginBody -> IO (Either String (AuthHeaders, LoginResponse))
login isDemo key logBody = do
  let options = loginOptions key
  response <- postWith options loginUrl (toJSON logBody)
  case response ^. responseStatus . statusCode of
       200  -> do
         let loginResponse = eitherDecode (response ^. responseBody) :: Either String LoginResponse
         case loginResponse of 
              Left e -> return $ Left e
              Right res -> 
                let tokens = getSecurityHeaders response
                in case tokens of
                        Just (cst, xst) -> do 
                          let clientToken = decodeUtf8 cst
                          let sessToken = decodeUtf8 xst
                          return $ 
                            Right (AuthHeaders clientToken sessToken key isDemo, res)
                        Nothing -> return $ Left "Could not parse tokens"
       _ -> apiError response Map.empty
  where loginUrl = cs $ host isDemo </> restPath


loginOptions :: Text -> Options
loginOptions key = baseHeaders "2" key


getSecurityHeaders :: Response BL.ByteString -> Maybe (BS.ByteString, BS.ByteString)
getSecurityHeaders response = (,) <$> cst <*> xst
  where cst = response ^? responseHeader "CST"
        xst = response ^? responseHeader "X-SECURITY-TOKEN"

-- | Log out of the API
logout :: AuthHeaders -> IO (Either ApiError ())
logout headers = do 
  let opts = buildHeaders "1" headers
  r <- deleteWith opts $ cs (host (isDemo headers) </> restPath)
  case r ^. responseStatus ^. statusCode of
       204 -> do return $ Right ()
       _   -> do return . Left $ decodeError (r ^. responseBody)


-- | Details of the current session as returned by GET /session Note that 
-- all of the fields in this object terminate with an apostrophe. This is to 
-- avoid name collisions with LoginResponse and will be updated in future 
-- releases, OverloadedRecordFields is implemented in GHC
data SessionDetails = SessionDetails { clientId' :: Text
                                     , accountId' :: Text
                                     , timezoneOffset' :: Int
                                     , locale' :: Text
                                     , currency' :: Text
                                     , lightstreamerEndpoint' :: Text
                                     } deriving (Eq, Generic, Show)

instance FromJSON SessionDetails where

  parseJSON = withObject "SessionDetails" $ \o -> do
    cId <- o .: "clientId"
    aId <- o .: "accountId"
    tzO <- o .: "timezoneOffset"
    loc <- o .: "locale"
    cny <- o .: "currency"
    lse <- o .: "lightstreamerEndpoint"
    return $ SessionDetails cId aId tzO loc cny lse


-- | Return an instance of SessionDetails for the active account
sessionDetails :: AuthHeaders -> IO (Either ApiError SessionDetails)
sessionDetails a@(AuthHeaders _ _ _ isLogin) = do
  let opts = buildHeaders "1" a
  r <- getWith opts (cs $ host isLogin <> restPath)
  let bod = r ^. responseBody
  case r ^. responseStatus ^. statusCode of
       200 -> do
         either (\_ -> error "Could not decode session details")
                (\sessionDetails -> do return $ Right sessionDetails)
                (eitherDecode bod)
       _ -> do
         return $ Left (decodeError bod)


-- Data required for a switchAccount request
data SwitchAccountData = SwitchAccountData Text Bool deriving (Eq, Generic, Show)

instance ToJSON SwitchAccountData where
  toJSON (SwitchAccountData accId defaultAccount) = 
    object [ "accountId" .= accId
           , "defaultAccount" .= defaultAccount
           ]


-- | Switch to a different user account. Untested as of 26/01/2016 since 403
-- error was constantly returned by the Api Companion when testing
switchAccount :: AuthHeaders -> Text -> Bool -> IO (Either String ())
switchAccount a@(AuthHeaders _ _ _ isLogin) id isDefault = do
  let payload = toJSON $ SwitchAccountData id isDefault
  let opts = buildHeaders "1" a
  r <- putWith opts (cs $ host isLogin </> restPath) payload
  case r ^. responseStatus ^. statusCode of
       200 -> return $ Right ()
       _   -> apiError r Map.empty 


accountsPath :: Bool -> String
accountsPath isLogin = cs $ host isLogin </> restPath


encryptionKey = undefined


tokenRefresh = undefined
