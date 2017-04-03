{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module IG.REST.Markets.Types where

import Data.Aeson
import Data.Text
import Data.Time
import IG.REST
import GHC.Generics


data MarketData = MarketData { nodes :: Maybe [Node]
                             , markets :: Maybe [Market]
                             } deriving (Generic, Show)


instance FromJSON MarketData


data Node = Node { id :: Text
                 , name :: Text
                 } deriving (Generic, Show)


instance FromJSON Node


data Market = Market { delayTime :: Integer -- ^ Price delay time in minutes
                     , bid :: Double -- ^ The bid price
                     , epic :: Text -- ^ Instrument identifier
                     , expiry :: Text -- ^ Instrument expiry period
                     , high :: Double -- ^ Highest price of the day
                     , instrumentName :: Text -- ^ The name of the instrument
                     , instrumentType :: InstrumentType 
                     , lotSize :: Double -- ^ Instrument lot size
                     , low :: Double -- ^ Lowest price of the day
                     , marketStatus :: MarketStatus -- ^ The current status of the market
                     , netChange :: Double -- ^ The price net change
                     , offer :: Double -- ^ The offer price
                     , otcTradeable :: Bool -- ^ True if otcTradeable
                     , percentageChange :: Double -- ^ The percentage price change on the day
                     , scalingFactor :: Double -- ^ Multiplying factor to determine actual pip value for the levels used by the instrument
                     , streamingPricesAvailable :: Bool -- ^ True if streaming prices are available (i.e. the market is tradeable and the client holds the necessary access permissions)
                     , updateTimeUTC :: IGTime
                     } deriving (Generic, Show)


instance FromJSON Market


data MarketStatus = CLOSED
                  | EDITS_ONLY
                  | OFFLINE
                  | ON_AUCTION
                  | ON_AUCTION_NO_EDITS
                  | SUSPENDED
                  | TRADEABLE
                  deriving (Generic, Show)


instance FromJSON MarketStatus


-- | Represents the content of a response from the /markets endpoint. 
data MarketDetails = MarketDetails { dealingRules :: DealingRules
                                   , instrument :: Instrument
                                   , snapshot :: SnapShot
                                   } deriving (Generic, Show)

instance FromJSON MarketDetails


-- | Represents the content of a DealingRules object
data DealingRules = DealingRules { marketOrderPreference :: MarketOrderPreference
                                 , maxStopOrLimitDistance :: DealingRule
                                 , minControlledRiskStopDistance :: DealingRule
                                 , minDealSize :: DealingRule
                                 , minNormalStopOrLimitDistance :: DealingRule
                                 , minStepDistance :: DealingRule
                                 , trailingStopsPreference :: TrailingStopsPreference
                                 } deriving (Generic, Show)


instance FromJSON DealingRules


-- | Represents the content of an Instrument object
data Instrument = Instrument { chartCode :: Maybe Text
                             , contractSize :: Maybe Text
                             , controlledRiskAllowed :: Bool
                             , country :: Text
                             , currencies :: [Currency]
                             , epic :: Text
                             , expiry :: InstrumentExpiry
                             , expiryDetails :: ExpiryDetails
                             , forceOpenAllowed :: Bool
                             , limitedRiskPremium :: DealingRule
                             , lotSize :: Double
                             , marginDepositBands :: [DepositBand]
                             , marginFactor :: Double
                             , marginFactorUnit :: UnitDimension
                             , marketId :: Text
                             , name :: Text
                             , newsCode :: Text 
                             , onePipMeans :: Maybe Text
                             , openingHours :: Maybe MarketHours
                             , rolloverDetails :: Maybe RollOverDetails
                             , slippageFactor :: SlippageFactor
                             , specialInfo :: Maybe [Text]
                             , sprintMarketsMaximumExpiryTime :: Maybe Double
                             , sprintMarketsMinimumExpiryTime :: Maybe Double
                             , stopsLimitAllowed :: Maybe Bool
                             , streamingPricesAvailable :: Bool
                             , instrumentType :: InstrumentType
                             , unit :: TradeUnit
                             , valueOfOnePip :: Maybe Text
                             } deriving (Show)


instance FromJSON Instrument where

  parseJSON = withObject "Instrument" $ \r -> do
    chartCode             <- r .:? "chartCode"
    contractSize          <- r .:? "contractSize" 
    controlledRiskAllowed <- r .: "controlledRiskAllowed" 
    country               <- r .: "country" 
    currencies            <- r .: "currencies" 
    epic                  <- r .: "epic" 
    expiry                <- r .: "expiry" 
    expiryDetails         <- r .: "expiryDetails" 
    forceOpenAllowed      <- r .: "forceOpenAllowed" 
    limitedRiskPremium    <- r .: "limitedRiskPremium" 
    lotSize               <- r .: "lotSize" 
    marginDepositBands    <- r .: "marginDepositBands" 
    marginFactor          <- r .: "marginFactor" 
    marginFactorUnit      <- r .: "marginFactorUnit" 
    marketId              <- r .: "marketId" 
    name                  <- r .: "name" 
    newsCode              <- r .: "newsCode" 
    onePipMeans           <- r .:? "onePipMeans" 
    openingHours          <- r .:? "openingHours" 
    rolloverDetails       <- r .:? "rolloverDetails" 
    slippageFactor        <- r .: "slippageFactor" 
    specialInfo           <- r .:? "specialInfo" 
    sprintMarketsMaximumExpiryTime <- r .:? "sprintMarketsMaximumExpiryTime" 
    sprintMarketsMinimumExpiryTime <- r .:? "sprintMarketsMaximumExpiryTime" 
    stopsLimitAllowed     <- r .:? "stopsLimitAllowed" 
    streamingPricesAvailable <- r .: "streamingPricesAvailable" 
    instrumentType        <- r .: "type" 
    unit                  <- r .: "unit" 
    valueOfOnePip         <- r .: "valueOfOnePip" 
    return $ Instrument{..}


data SnapShot = SnapShot{ bid :: Double
                        , binaryOdds :: Maybe Double
                        , controlledRiskExtraSpread :: Double
                        , decimalPlacesFactor :: Double
                        , delayTime :: Double
                        , high :: Double
                        , low :: Double
                        , marketStatus :: MarketStatus
                        , netChange :: Double
                        , offer :: Double
                        , percentageChange :: Double
                        , scalingFactor :: Double
                        , updateTime :: IGTime
                        } deriving (Generic, Show)


instance FromJSON SnapShot


data TradeUnit = AMOUNT
               | CONTRACT
               | SHARES
               deriving (Generic, Show)


instance FromJSON TradeUnit

data SlippageFactor = SlippageFactor { unit :: Text
                                     , value :: Double
                                     } deriving (Generic, Show)


instance FromJSON SlippageFactor


data RollOverDetails = RollOverDetails { lastRolloverTime :: IGTime
                                       , settlementInfo :: Text
                                       } deriving (Generic, Show)


instance FromJSON RollOverDetails


data MarketHours = MarketHours { openTime :: IGTime
                               , closeTime :: IGTime
                               } deriving (Generic, Show)


instance FromJSON MarketHours


data UnitDimension = PERCENTAGE
                   | POINTS
                   deriving (Generic, Show)


instance FromJSON UnitDimension


data DepositBand = DepositBand { currency :: Text
                               , margin :: Double
                               , max :: Double
                               , min :: Double
                               } deriving (Generic, Show)


instance FromJSON DepositBand


data DealingRule = DealingRule { unit :: UnitDimension
                               , value :: Double
                               } deriving (Generic, Show)


instance FromJSON DealingRule


data ExpiryDetails = ExpiryDetails { lastDealingDate :: IGDate
                                   , settlementInfo :: Text
                                   } deriving (Generic, Show)

instance FromJSON ExpiryDetails


data Currency = Currency { baseExchangeRate :: Double
                         , code :: Text
                         , exchangeRate :: Double
                         , isDefault :: Bool
                         , symbol :: String
                         } deriving (Generic, Show)


instance FromJSON Currency


data TrailingStopsPreference = AVAILABLE
                             | NOT_AVAILABLE
                             deriving (Generic, Show)


instance FromJSON TrailingStopsPreference 


data MarketOrderPreference = AVAILABLE_DEFAULT_OFF
                           | AVAILABLE_DEFAULT_ON
                           | MO_NOT_AVAILABLE
                           deriving (Generic, Show)


instance FromJSON MarketOrderPreference where

  parseJSON = withText "MarketOrderPreference" $ \p ->
    case p of 
         "AVAILABLE_DEFAULT_OFF" -> return AVAILABLE_DEFAULT_OFF
         "AVAILABLE_DEFAULT_ON"  -> return AVAILABLE_DEFAULT_ON
         "NOT_AVAILABLE"         -> return MO_NOT_AVAILABLE


data HistoricalPrices = HistoricalPrices { instrumentType :: InstrumentType
                                         , metadata :: HistoricalMetadata
                                         , prices :: [ HistoricalPrice ]
                                         } deriving (Generic, Show)


instance FromJSON HistoricalPrices


data HistoricalMetadata = HistoricalMetadata { pageData :: PageData
                                             , size :: Double
                                             , allowance :: DataAllowance
                                             } deriving (Generic, Show)


instance FromJSON HistoricalMetadata


data PageData = PageData { pageNumber :: Int
                         , pageSize :: Double
                         , totalPages :: Double
                         } deriving (Generic, Show)


instance FromJSON PageData


data DataAllowance = DataAllowance { allowanceExpiry :: Double
                                   , remainingAllowance :: Double
                                   , totalAllowance :: Double
                                   } deriving (Generic, Show)


instance FromJSON DataAllowance


data HistoricalPrice = HistoricalPrice { closePrice :: PriceDatum
                                       , highPrice :: PriceDatum
                                       , lastTradedVolume :: Double
                                       , lowPrice :: PriceDatum
                                       , snapshotTimeUTC :: IGDate
                                       } deriving (Generic, Show)


instance FromJSON HistoricalPrice


data PriceDatum = PriceDatum { ask :: Double
                             , bid :: Double
                             , lastTraded :: Maybe Double
                             } deriving (Generic, Show)


instance FromJSON PriceDatum


data Resolution = DAY
                | HOUR
                | HOUR_2
                | HOUR_3
                | HOUR_4
                | MINUTE
                | MINUTE_10
                | MINUTE_15
                | MINUTE_2
                | MINUTE_3
                | MINUTE_30
                | MINUTE_5
                | MONTH
                | SECOND
                | WEEK
                deriving (Show)


data HistoryOpts = HistoryOpts { from :: Maybe UTCTime
                               , to   :: Maybe UTCTime
                               , max  :: Maybe Int
                               , pageSize :: Maybe Int
                               , resolution :: Maybe Resolution
                               , pageNumber :: Maybe Int
                               }

