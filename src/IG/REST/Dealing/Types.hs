{-# LANGUAGE DeriveGeneric #-}

module IG.REST.Dealing.Types where

import Data.Aeson
import Data.Monoid
import GHC.Generics
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Maybe
import Data.Time


utcDateTimeFormat :: String
utcDateTimeFormat = iso8601DateFormat $ Just "%H:%M:%S%Q"


-- | Represents the payload returned by a deal confirmation request to the API
data DealConfirmation = DealConfirmation { date :: UTCDate -- ^ Date and time of transaction
                                         , affectedDeals :: [Deal] -- ^ Affected Deals
                                         , dealId :: Text -- ^ Deal Identifier
                                         , dealReference :: Text -- ^ Deal Reference
                                         , dealStatus :: DealStatus -- ^ Deal status
                                         , direction :: Direction -- ^ Deal direction
                                         , epic :: Text -- ^ Instrument epic identifier
                                         , expiry :: InstrumentExpiry -- ^ Instrument expiry
                                         , guaranteedStop :: Bool -- ^ True if guaranteed stop
                                         , level :: Maybe Double -- ^ The level
                                         , limitDistance :: Maybe Double -- ^ Limit distance
                                         , limitLevel :: Maybe Double -- ^ The limit level
                                         , profit :: Maybe Double -- ^ Profit
                                         , profitCurrency :: Maybe Text
                                         , reason :: StatusReason 
                                         -- ^ The error (or success condition) for the
                                         -- trading operation
                                         , size :: Double
                                         , status :: Status
                                         , stopDistance :: Maybe Double -- ^ Stop distance
                                         , stopLevel :: Maybe Double -- ^ Stop level
                                         , trailingStop :: Bool -- ^ True if trailing stop
                                         } deriving (Generic, Show)

instance FromJSON DealConfirmation


data UTCDate = UTCDate UTCTime
             deriving (Show)


instance FromJSON UTCDate where
  parseJSON = withText "UTCDate" $ \d ->
    return $ UTCDate . fromJust $ time ( Text.unpack d )
    where time = parseTimeM True locale utcDateTimeFormat
          locale = defaultTimeLocale


-- | Represents a time point as returned by the API
data DealTime = DealTime UTCTime 
              deriving (Show)


instance FromJSON DealTime where 

  parseJSON = withText "DealTime" $ \t -> 
    return $ DealTime . fromJust $ time ( Text.unpack t )
    where time = parseTimeM True locale "%X" 
          locale = defaultTimeLocale

-- | Represents overview info on an account's deals. 
data Deal = Deal { dealId :: String
                 , status :: Status
                 }
          deriving (Show, Generic)

instance FromJSON Deal where
  
  parseJSON = withObject "Deal" $ \o -> do
    id <- o .: "dealId"
    s  <- o .: "status"
    return $ Deal id s


-- | Indicates the status of a deal
data DealStatus = ACCEPTED
                | REJECTED
                deriving (Generic, Show)

instance FromJSON DealStatus


data Status = AMENDED
            | DELETED
            | FULLY_CLOSED
            | OPEN
            | OPENED
            | PARTIALLY_CLOSED
            deriving (Generic, Show)

instance FromJSON Status


-- | Indicates the direction that the trade is betting on. BUY = bullish, SELL 
-- = bearish. I'm sure you get the picture. 
data Direction = BUY
               | SELL
               deriving (Eq, Generic, Show)

instance FromJSON Direction


instance ToJSON Direction

-- | The reasons that a trade operation has succeeded or failed. 
data StatusReason = ACCOUNT_NOT_ENABLED_TO_TRADING -- ^ The account is not enabled to trade
                  | ATTACHED_ORDER_LEVEL_ERROR -- ^ The level of the attached stop of limit is not valid
                  | ATTACHED_ORDER_TRAILING_STOP_ERROR -- ^ The trailing stop value is invalid
                  | CANNOT_CHANGE_STOP_TYPE -- ^ Cannot change the stop type.
                  | CANNOT_REMOVE_STOP -- ^ Cannot remove the stop.
                  | CLOSING_ONLY_TRADES_ACCEPTED_ON_THIS_MARKET -- ^ IG are not taking opening deals on a Controlled Risk basis on this market
                  | CONFLICTING_ORDER -- ^ Resubmitted request does not match the original order.
                  | CONTACT_SUPPORT_INSTRUMENT_ERROR -- ^ Instrument has an error - check the order's currency is the instrument's currency (see the market's details); otherwise please contact support.
                  | CR_SPACING -- ^ IG are unable to process this order. The stop or limit level you have requested is not a valid trading level in the underlying market.
                  | DUPLICATE_ORDER_ERROR -- ^ The order has been rejected as it is a duplicate of a previously issued order
                  | EXCHANGE_MANUAL_OVERRIDE -- ^ Exchange check failed. Please call in for assistance.
                  | EXPIRY_LESS_THAN_SPRINT_MARKET_MIN_EXPIRY -- ^ Order expiry is less than the sprint market's minimum expiry. Check the sprint market's market details for the allowable expiries.
                  | FINANCE_REPEAT_DEALING -- ^ The total size of deals placed on this market in a short period has exceeded our limits. Please wait before attempting to open further positions on this market.
                  | FORCE_OPEN_ON_SAME_MARKET_DIFFERENT_CURRENCY -- ^ Ability to force open in different currencies on same market not allowed
                  | GENERAL_ERROR -- ^ an error has occurred but no detailed information is available. Check transaction history or contact support for further information
                  | GOOD_TILL_DATE_IN_THE_PAST -- ^ The working order has been set to expire on a past date
                  | INSTRUMENT_NOT_FOUND -- ^ The requested market was not found
                  | INSUFFICIENT_FUNDS -- ^ The account has not enough funds available for the requested trade
                  | LEVEL_TOLERANCE_ERROR -- ^ The market level has moved and has been rejected
                  | LIMIT_ORDER_WRONG_SIDE_OF_MARKET -- ^ The deal has been rejected because the limit level is inconsistent with current market price given the direction.
                  | MANUAL_ORDER_TIMEOUT -- ^ The manual order timeout limit has been reached
                  | MARGIN_ERROR -- ^ Order declined during margin checks Check available funds.
                  | MARKET_CLOSED -- ^ The market is currently closed
                  | MARKET_CLOSED_WITH_EDITS -- ^ The market is currently closed with edits
                  | MARKET_CLOSING -- ^ The epic is due to expire shortly, client should deal in the next available contract.
                  | MARKET_NOT_BORROWABLE -- ^ The market does not allow opening shorting positions
                  | MARKET_OFFLINE -- ^ The market is currently offline
                  | MARKET_ORDERS_NOT_ALLOWED_ON_INSTRUMENT -- ^ The epic does not support 'Market' order type
                  | MARKET_PHONE_ONLY -- ^ The market can only be traded over the phone
                  | MARKET_ROLLED -- ^ The market has been rolled to the next period
                  | MARKET_UNAVAILABLE_TO_CLIENT -- ^ The requested market is not allowed to this account
                  | MAX_AUTO_SIZE_EXCEEDED -- ^ The order size exceeds the instrument's maximum configured value for auto-hedging the exposure of a deal
                  | MINIMUM_ORDER_SIZE_ERROR -- ^ The order size is too small
                  | MOVE_AWAY_ONLY_LIMIT -- ^ The limit level you have requested is closer to the market level than the existing stop. When the market is closed you can only move the limit order further away from the current market level.
                  | MOVE_AWAY_ONLY_STOP -- ^ The stop level you have requested is closer to the market level than the existing stop level. When the market is closed you can only move the stop level further away from the current market level
                  | MOVE_AWAY_ONLY_TRIGGER_LEVEL -- ^ The order level you have requested is moving closer to the market level than the exisiting order level. When the market is closed you can only move the order further away from the current market level.
                  | OPPOSING_DIRECTION_ORDERS_NOT_ALLOWED -- ^ Opening CR position in opposite direction to existing CR position not allowed.
                  | OPPOSING_POSITIONS_NOT_ALLOWED -- ^ The deal has been rejected to avoid having long and short open positions on the same market or having long and short open positions and working orders on the same epic
                  | ORDER_DECLINED -- ^ Order declined; please contact Support
                  | ORDER_LOCKED -- ^ The order is locked and cannot be edited by the user
                  | ORDER_NOT_FOUND -- ^ The order has not been found
                  | OVER_NORMAL_MARKET_SIZE -- ^ The total position size at this stop level is greater than the size allowed on this market. Please reduce the size of the order.
                  | PARTIALY_CLOSED_POSITION_NOT_DELETED -- ^ Position cannot be deleted as it has been partially closed.
                  | POSITION_ALREADY_EXISTS_IN_OPPOSITE_DIRECTION -- ^ The deal has been rejected because of an existing position. Either set the 'force open' to be true or cancel opposing position
                  | POSITION_NOT_AVAILABLE_TO_CANCEL -- ^ Position cannot be cancelled. Check transaction history or contact support for further information.
                  | POSITION_NOT_AVAILABLE_TO_CLOSE -- ^ Cannot close this position. Either the position no longer exists, or the size available to close is less than the size specified.
                  | POSITION_NOT_FOUND -- ^ The position has not been found
                  | REJECT_CFD_ORDER_ON_SPREADBET_ACCOUNT -- ^ Invalid attempt to submit a CFD trade on a spreadbet account
                  | REJECT_SPREADBET_ORDER_ON_CFD_ACCOUNT -- ^ Invalid attempt to submit a spreadbet trade on a CFD account
                  | SIZE_INCREMENT -- ^ Order size is not an increment of the value specified for the market.
                  | SPRINT_MARKET_EXPIRY_AFTER_MARKET_CLOSE -- ^ The expiry of the position would have fallen after the closing time of the market
                  | STOP_OR_LIMIT_NOT_ALLOWED -- ^ The market does not allow stop or limit attached orders
                  | STOP_REQUIRED_ERROR -- ^ The order requires a stop
                  | STRIKE_LEVEL_TOLERANCE -- ^ The submitted strike level is invalid
                  | SUCCESS -- ^ The operation completed successfully
                  | TRAILING_STOP_NOT_ALLOWED -- ^ The market or the account do not allow for trailing stops
                  | UNKNOWN -- ^ The operation resulted in an unknown result condition. Check transaction history or contact support for further information
                  | WRONG_SIDE_OF_MARKET -- ^ The requested operation has been attempted on the wrong direction
                  deriving (Generic, Show)

instance FromJSON StatusReason


-- | Represents the information returned by the api for a single position following
-- a call to deal/positions
data PositionData = PositionData { position :: Position -- ^ Details of the position
                                 , market :: Market -- ^ Details of the market it is in
                                 } deriving (Generic, Show)

instance FromJSON PositionData


data Market = Market { bid :: Double -- ^ Bid
                     , delayTime :: Double -- ^ Instrument price delay (minutes)
                     , epic :: Text -- ^ Instrument epic identifier
                     , expiry :: InstrumentExpiry -- ^ Instrument expiry period
                     , high :: Double -- ^ High price
                     , instrumentName :: Text -- ^ Instrument name
                     , instrumentType :: InstrumentType -- ^ Instrument Type
                     , lotSize :: Double -- ^ Instrument lot size
                     , low :: Double -- ^ Low price
                     , marketStatus :: MarketStatus -- ^ Describes the current status of a given market
                     , netChange :: Double -- ^ Price net change
                     , offer :: Double -- ^ Offer
                     , percentageChange :: Double -- ^ Price percentage change
                     , scalingFactor :: Double -- ^ multiplying factor to determine actual pip value for the levels used by the instrument
                     , streamingPricesAvailable :: Bool -- ^ True if streaming prices are available, i.e. the market is tradeable and the client has appropriate permissions
                     , updateTimeUTC :: DealTime -- ^ Time of last instrument price update
                     } deriving (Show)


instance FromJSON Market where
  parseJSON = withObject "Market" $ \o -> do
    bid <- o .: "bid"
    dt  <- o .: "delayTime"
    ep  <- o .: "epic"
    ex  <- o .: "expiry"
    hi  <- o .: "high"
    inN <- o .: "instrumentName"
    inT <- o .: "instrumentType"
    ls  <- o .: "lotSize"
    l   <- o .: "low"
    mS  <- o .: "marketStatus"
    nC  <- o .: "netChange"
    off <- o .: "offer"
    pC  <- o .: "percentageChange"
    sF  <- o .: "scalingFactor"
    sPA <- o .: "streamingPricesAvailable"
    utc <- o .: "updateTimeUTC"
    return $ Market bid dt ep ex hi inN inT ls l mS nC off pC sF sPA utc


data MarketStatus = CLOSED
                  | EDITS_ONLY
                  | OFFLINE 
                  | ON_AUCTION
                  | ON_AUCTION_NO_EDITS
                  | SUSPENDED
                  | TRADEABLE
                  deriving (Generic, Show)

instance FromJSON MarketStatus

data InstrumentType = BINARY -- ^ Binaries
                    | BUNGEE_CAPPED -- ^ Capped bungees
                    | BUNGEE_COMMODITIES -- ^ Commodity bungees
                    | BUNGEE_CURRENCIES -- ^ Currency bungees
                    | BUNGEE_INDICES -- ^ Index bungees
                    | COMMODITIES -- ^ Commodities
                    | CURRENCIES -- ^ Currencies
                    | INDICES -- ^ Indices
                    | OPT_COMMODITIES -- ^ Commodity options
                    | OPT_CURRENCIES -- ^ Currency options
                    | OPT_INDICES -- ^ Index options
                    | OPT_RATES -- ^ FX Options
                    | OPT_SHARES -- ^ Share options
                    | RATES -- ^ Rates
                    | SECTORS -- ^ Sectors
                    | SHARES -- ^ Shares
                    | SPRINT_MARKET -- ^ Sprint Market
                    | TEST_MARKET -- ^ Test market
                    | UNKNOWN_INSTRUMENT_TYPE -- ^ Unknown
                    deriving (Generic, Show)

instance FromJSON InstrumentType where

  parseJSON = withText "InstrumentType" $ \t -> 
    case t of
         "BUNGEE_CAPPED" ->       return BUNGEE_CAPPED 
         "BUNGEE_COMMODITIES" ->  return BUNGEE_COMMODITIES 
         "BUNGEE_CURRENCIES" ->   return BUNGEE_CURRENCIES 
         "BUNGEE_INDICES" ->      return BUNGEE_INDICES 
         "COMMODITIES" ->         return COMMODITIES 
         "CURRENCIES" ->          return CURRENCIES 
         "INDICES" ->             return INDICES 
         "OPT_COMMODITIES" ->     return OPT_COMMODITIES 
         "OPT_CURRENCIES" ->      return OPT_CURRENCIES 
         "OPT_INDICES" ->         return OPT_INDICES 
         "OPT_RATES" ->           return OPT_RATES 
         "OPT_SHARES" ->          return OPT_SHARES
         "RATES" ->               return RATES 
         "SECTORS" ->             return SECTORS 
         "SHARES" ->              return SHARES 
         "SPRINT_MARKET" ->       return SPRINT_MARKET 
         "TEST_MARKET" ->         return TEST_MARKET 
         "UNKNOWN" ->             return UNKNOWN_INSTRUMENT_TYPE
         
-- ^ Data on a position. 
data Position = Position { contractSize :: Double -- ^ Size of the contract
                         , controlledRisk :: Bool -- ^ True if position is risk controlled
                         , createdDateUTC :: UTCDate -- ^ Date the position was opened
                         , currency :: Text -- ^ Position currency ISO code
                         , dealId :: Text -- ^ Deal identifier
                         , dealReference :: Text -- ^ Deal reference
                         , direction :: Direction -- ^ Deal direction
                         , level :: Double -- ^ Level at which position was opened
                         , limitLevel :: Maybe Double -- ^ Limit Level
                         , size :: Double -- ^ Deal size
                         , stopLevel :: Double -- ^ Stop level
                         , trailingStep :: Maybe Double -- ^ Trailing step size
                         , trailingStopDistance :: Maybe Double -- ^ Trailing stop distance
                         } deriving (Generic, Show)

instance FromJSON Position


-- ^ Represents the payload that is sent over when a position is created
data PositionRequest = PositionRequest { currencyCode :: Text
                                       , dealReference :: Maybe Text
                                       , direction :: Direction
                                       , epic :: Text
                                       , expiry :: InstrumentExpiry
                                       , forceOpen :: Bool
                                       , guaranteedStop :: Bool
                                       , level :: Maybe Double
                                       , limitDistance :: Maybe Double
                                       , limitLevel :: Maybe Double
                                       , orderType :: OrderType
                                       , quoteId :: Maybe Text
                                       , size :: Double
                                       , stopLevel :: Maybe Double
                                       , timeInForce :: TimeInForce
                                       , trailingStop :: Bool
                                       , trailingStopIncrement :: Maybe Double
                                       } deriving (Show, Generic)

instance ToJSON PositionRequest

instance FromJSON PositionRequest


-- ^ Specifies the expiry of the instrument which you wish to purchase. The date
-- (and sometimes time) at which a spreadbet or CFD will automatically close 
-- against some predefined market value should the bet remain open beyond its 
-- last dealing time. Some CFDs do not expire, and have an expiry of '-'. eg 
-- DEC-14, or DFB for daily funded bets
data InstrumentExpiry = DFB
                      | None
                      | Expires Text Text
                      deriving (Show)

instance ToJSON InstrumentExpiry where

  toJSON (Expires m d) = String $ m <> "-" <> d
  toJSON None = String "-"
  toJSON x = String . Text.pack . show $ x

instance FromJSON InstrumentExpiry where

  parseJSON = withText "InstrumentExpiry" $ \t -> 
    case t of
         "DFB" -> return DFB
         "-"   -> return None
         ie    -> do 
                  let expiryParts = Text.splitOn "-" ie
                  case expiryParts of
                       [] -> fail "No expiry data provided"
                       (month:year:_) -> return $ Expires month year
                       (x:_) -> fail . Text.unpack $ "Only one part of expiry data provided: " <> x


data TimeInForce = FILL_OR_KILL
                 | EXECUTE_AND_ELIMINATE
                 deriving (Show, Generic)

instance ToJSON TimeInForce

instance FromJSON TimeInForce


-- | Describes the order level model to be used for a position operation
data OrderType = LIMIT  -- ^ Limit orders get executed at the price seen by IG at
                        -- the moment of booking a trade
                        -- A limit determines the level at which the order or the
                        -- remainder of the order will be rejected
               | MARKET -- ^ Market orders get executed at the price seen by
                        -- IG at the time of booking the trade. A level cannot
                        -- be specified. Not applicable to BINARY instruments.
               | QUOTE  -- ^ Quote orders get executed at the specified level. 
                        -- The level has to be accompanied by a valid quote id.
                        -- This type is only available subject to agreement with
                        -- IG.
               deriving (Generic, Show)

instance ToJSON OrderType

instance FromJSON OrderType


data DealReference = DealReference { dealReference :: Text
                                   } deriving (Generic, Show)

instance FromJSON DealReference


data PositionsResponse = PositionsResponse { positions :: [PositionData] 
                                           } deriving (Generic, Show)

instance FromJSON PositionsResponse


data ClosePositionRequest 
  = ClosePositionRequest { dealId :: Text -- ^ The dealId of the position to be closed
                         , epic :: Maybe Text
                         , expiry :: Maybe InstrumentExpiry
                         , direction :: Direction
                         , level :: Maybe Double
                         , orderType :: OrderType
                         , timeInForce :: Maybe TimeInForce
                         , quoteId :: Maybe Text
                         , size :: Double
                         } deriving (Generic, Show)

instance ToJSON ClosePositionRequest
                         

-- | Defines the options that can be set when closing a deal. CloseOptions is designed
-- to work in conjunction with PositionData when closePosition is applied to them
-- such that the library copies the data from the correct sources. 
data CloseOptions = CloseOptions { level :: Maybe Double
                                 , orderType :: OrderType
                                 , timeInForce :: Maybe TimeInForce
                                 , quoteId :: Maybe Text
                                 , size :: Maybe Double
                                 , expiry :: Maybe InstrumentExpiry
                                 , epic :: Maybe Text
                                 } 


-- | Options for payload used in updating a position. The API only allows the 
-- updating of stop and limit levels
data PositionUpdateRequest = PositionUpdateRequest { limitLevel :: Maybe Double
                                                   , stopLevel :: Maybe Double
                                                   , trailingStop :: Maybe Bool
                                                   , trailingStopDistance :: Maybe Double
                                                   , trailingStopIncrement :: Maybe Double
                                                   } deriving (Generic, Show)

instance ToJSON PositionUpdateRequest