{-# LANGUAGE DeriveGeneric #-}
-- | Functions for working with the Login Endpoint.

module IG.REST.Login where

import Data.Text
import Data.Scientific
import GHC.Generics
-- Actions to create: 
-- Log in
-- Log out
-- Session details
-- switch account
-- get encryption key
-- refresh the token


-- | Represents the payload returned by the API when the user successfully logs
-- in.
data LoginResponse = LoginResponse { 
                                   -- | The Client Security Token
                                   cst :: Text
                                   -- | The account session security access token
                                   , securityToken :: Text
                                   -- | AccountInfo object
                                   , accountInfo :: AccountInfo
                                   -- | Account type
                                   , accountType :: AccountType
                                   -- | Array of details for each user account
                                   , accounts :: [AccountDetails]
                                   -- | Client Identifier
                                   , clientId :: Text
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
                                   , reroutingEnvironment :: ReroutingEnvironment
                                   -- | Client account timezone offset relative to UTC, expressed in hours
                                   , timeZoneOffset :: Integer
                                   -- | True if client can use trailing stops
                                   , trailingStopsEnabled :: Bool
                                   } deriving (Generic)

                                  
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
                               } deriving (Generic)

-- | The three different types of account that IG support
data AccountType = CFD
                 | Physical
                 | SpreadBet
                 deriving (Generic)
                                  

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
                                     }


data ReroutingEnvironment = DEMO
                          | LIVE
                          | TEST
                          | UAT
login = undefined

logout :: IO ()
logout = undefined

sessionDetails = undefined

switchAccount = undefined

encryptionKey = undefined

tokenRefresh = undefined
