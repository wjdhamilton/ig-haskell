{-# LANGUAGE DeriveGeneric #-}

module IG.REST.Accounts.Types where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics


data Account = Account { accountAlias :: Maybe Text
                       , accountId :: String
                       , accountName :: String
                       , accountType :: AccountType
                       , balance :: AccountBalances
                       , canTransferFrom :: Bool
                       , canTransferTo :: Bool
                       , currency :: Text
                       , preferred :: Bool
                       , status :: AccountStatus
                       } deriving (Generic, Show)

instance ToJSON Account

instance FromJSON Account


data AccountType = CFD 
                 | PHYSICAL
                 | SPREADBET
                 deriving (Generic, Show)

instance ToJSON AccountType

instance FromJSON AccountType


data AccountBalances = AccountBalances { available :: Double
                                       , balance :: Double
                                       , deposit :: Double
                                       , profitLoss :: Double
                                       } deriving (Generic, Show)

instance FromJSON AccountBalances

instance ToJSON AccountBalances


data AccountStatus = DISABLED
                   | ENABLED
                   | SUSPENDED_FROM_DEALING
                   deriving (Generic, Show)

instance FromJSON AccountStatus

instance ToJSON AccountStatus
