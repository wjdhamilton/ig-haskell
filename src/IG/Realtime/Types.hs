module IG.Realtime.Types where

import Data.Char
import Data.List as List
import Data.Monoid
import Data.String.Conversions
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time
import IG
import Network.Wreq

data StreamProperty = Password Text -- ^ The lightstreamer password, see @Realtime#tokens
                    | MaxBandwidth Int -- ^ Optional. 
                    | ContentLength Int -- ^ Optional. Content-Length to be used for the connection.
                    | KeepAliveMillis Int -- ^ Optional. Longest inactivity time allowed for the connection.
                    | ReportInfo Bool -- ^ Optional. 
                    | Polling Bool -- ^ If True, requests a polling connection
                    | PollingMillis Int -- ^ Expected time between closing the last connection and opening a new one to poll the session.
                    | IdleMillis Int -- ^ Time the server is allowed to wait for an update to return.
                    

encodeStreamProperty :: StreamProperty -> Text
encodeStreamProperty (Password t)        = flatten ["LS_password",t]
encodeStreamProperty (MaxBandwidth n)    = flatten ["LS_requested_max_bandwidth", cs . show $ n]
encodeStreamProperty (ContentLength n)   = flatten ["LS_content_length", cs . show $ n]
encodeStreamProperty (KeepAliveMillis n) = flatten ["LS_keepalive_millis",  cs . show $ n]
encodeStreamProperty (ReportInfo n)      = flatten ["LS_report_info", cs . show $ n]
encodeStreamProperty (Polling isPoll)    = flatten ["LS_polling", cs . show $ isPoll]
encodeStreamProperty (PollingMillis n)   = flatten ["LS_polling_millis", cs . show $ n]
encodeStreamProperty (IdleMillis n)      = flatten ["LS_idle_millis", cs . show $ n]

flatten :: [Text] -> Text
flatten = mconcat . List.intersperse "="


-- | The different attributes that are understood by the IG Realtime system
data ControlProperty = SessionId Text -- ^ Mandatory. The session id
                     | Table Int -- ^ Mandatory. The number of a table to be created or changed
                     | Op StreamOp -- ^ Mandatory. 
                     | DataAdapter Text -- ^ Usually not required
                     | Selector Text
                     | Mode SubMode -- ^ Mandatory. 
                     | ReqBufferSize Int
                     | ReqMaxFrequency Frequency
                     | Snapshot SnapshotAtt

-- | Encode the value of a ControlProperty such that it can be used in a
-- request to the IG server
encodeProperty :: ControlProperty -> FormParam
encodeProperty (SessionId s)       = "LS_session" := s
encodeProperty (Table n)           = "LS_table" := n
encodeProperty (Op o)              = "LS_op" := encode o
encodeProperty (DataAdapter n)     = "LS_data_adapter" := n
encodeProperty (Selector s)        = "LS_selector" := s
encodeProperty (Mode m)            = "LS_mode" := encode m
encodeProperty (ReqBufferSize n)   = "LS_requested_buffer_size" := show n
encodeProperty (ReqMaxFrequency f) = "LS_requested_max_frequency" := encode f
encodeProperty (Snapshot att)     = "LS_requested_max_frequency" := encode att


class (Eq a, Ord a, Show a) => ControlAttribute a where
  encode :: a -> Text
  encode = Text.toUpper . snakeCase . toText


-- | Options for adding a table to a subscription
data StreamOp = Add 
              | AddSilent 
              | Start 
              | Delete
              deriving (Eq, Ord, Show)


instance ControlAttribute StreamOp where
  encode = Text.toLower . toText


data SubMode = Merge
             | Distinct
             deriving (Eq, Ord, Show)

instance ControlAttribute SubMode

type AccountId = Text


-- | Fields that can be returned in a data feed
data Schema = Market Epic [MarketFields]
            | SprintMarket Epic [SprintFields]
            | Account AccountId [AccountFields]
            | Trade AccountId [TradeFields]
            | Chart Epic TimeSlice [ChartFields]
            | ChartTick Epic [TickFields]


encodeSchema :: Schema -> [FormParam]
encodeSchema s@(Market _ fs)        = [schemaId s, schemaParams fs]
encodeSchema s@(SprintMarket _ fs)  = [schemaId s, schemaParams fs]
encodeSchema s@(Account _ fs)       = [schemaId s, schemaParams fs]
encodeSchema s@(Trade _ fs)         = [schemaId s, schemaParams fs]
encodeSchema s@(Chart _ _ fs)       = [schemaId s, schemaParams fs]
encodeSchema s@(ChartTick _ fs)     = [schemaId s, schemaParams fs]


schemaId :: Schema -> FormParam
schemaId (Market e _) = "LS_id" := "MARKET:" <> e
schemaId (SprintMarket e _) = "LS_id" := "MARKET:" <> e
schemaId (Account ident _) = "LS_id" := "ACCOUNT:" <> ident
schemaId (Trade ident _) = "LS_id" := "TRADE:" <> ident
schemaId (Chart e t  _) = "LS_id" := "CHART:" <> e <> ":" <> encode t
schemaId (ChartTick e _) = "LS_id" := "CHART:" <> e <> ":TICK"



schemaParams :: ControlAttribute a => [a] -> FormParam
schemaParams vals = "LS_schema" := encVals vals
  where encVals = Text.concat . intersperse " " . map encode


data Frequency = Unfiltered
               | Maximum Int
               deriving (Eq, Ord, Show)


instance ControlAttribute Frequency


-- | Fields that can be returned for a Market 
data MarketFields = Bid
                  | Offer
                  | High
                  | Low
                  | MidOpen
                  | Change
                  | ChangePct
                  | MarketDelay
                  | MarketState
                  | UpdateTime
                  deriving (Eq, Ord, Show)


instance ControlAttribute MarketFields


-- | Fields that can be returned for a SprintMarket
data SprintFields = StrikePrice
                  | SprintMarketState
                  | Odds
                  deriving (Eq, Ord, Show)


instance ControlAttribute SprintFields


-- | Fields that can be returned for an Account
data AccountFields = Pnl
                   | Deposit
                   | AvailableCash
                   | PnlLr
                   | PnlNlr
                   | Funds
                   | Margin
                   | MarginLr
                   | MarginNLr
                   | AvailableToDeal
                   | Equity
                   | EquityUsed
                   deriving (Eq, Ord, Show)

instance ControlAttribute AccountFields


-- | Fields that can be returned 
data TradeFields = Confirms
                 | Opu
                 | Wou
                 | Heartbeat
                 deriving (Eq, Ord, Show)


instance ControlAttribute TradeFields


data ChartFields = Ltv
                 | Ttv
                 | Utm -- ^ Update time as milliseconds from epoch
                 | DayOpenMid
                 | DayNetChgMid
                 | DayPercChgMid
                 | DayHigh
                 | DayLow
                 | OfrOpen
                 | OfrHigh
                 | OfrLow
                 | OfrClose
                 | BidOpen
                 | BidHigh
                 | BidLow
                 | BidClose
                 | LtpOpen
                 | LtpHigh
                 | LtpLow
                 | LtpClose
                 | ConsEnd
                 | ConsTickCount
                 deriving (Eq, Ord, Show)

instance ControlAttribute ChartFields


data TimeSlice = Second | Minute1 | Minute5 | Hour deriving (Eq, Ord, Show)


instance ControlAttribute TimeSlice where
  encode Minute1 = "1MINUTE"
  encode Minute5 = "5MINUTE"
  encode x       = Text.toUpper . cs $ show x


data TickFields = Ofr
                | Tbid
                | Ltp
                | Tltv
                | Tttv
                | Tutm
                | TDayOpenMid
                | TDayNetChgMid
                | TDayPercChgMid
                | TDayHigh
                | TDayLow
                deriving (Eq, Ord, Show)


instance ControlAttribute TickFields where

  encode Ofr            = "OFR"
  encode Tbid           = "BID"
  encode Ltp            = "LTP"
  encode Tltv           = "LTV"
  encode Tttv           = "TTV"
  encode Tutm           = "UTM"
  encode TDayOpenMid    = "DAY_OPEN_MID"
  encode TDayNetChgMid  = "DAY_NET_CHG_MID"
  encode TDayPercChgMid = "DAY_PERC_CHG_MID"
  encode TDayHigh       = "DAY_HIGH"
  encode TDayLow        = "DAY_LOW"


data SnapshotAtt = UseSnapshot Bool
                 | Length Int
                 deriving (Eq, Ord, Show)


instance ControlAttribute SnapshotAtt where
  encode (UseSnapshot perhaps) = Text.toLower . toText $ perhaps
  encode (Length n)            = toText n


toText :: Show a => a -> Text
toText = cs . show


snakeCase :: Text -> Text
snakeCase t =  cs . tail . snake . cs $ t
  where snake []     = []
        snake (c:chars) = if isUpper c then '_' : c : snake chars
                                    else c : snake chars

type TableNo = Int
type ItemNo = Int


data LSValue = LSValue TableNo ItemNo [Maybe Text] deriving (Show)


-- | Represents the kinds of things that the IG server can send. 
data StreamContent = Update LSValue -- ^ A data update
                   | Beat UTCTime -- ^ A hearteat message, with its timestamp
                   | Timedout -- ^ The IG server has timed out
                   | Exhausted -- ^ The session has been exhausted. This is handled by reopening the session
                   | CannotRebind RealTimeError-- ^ The stream has been exhausted, and rebind has failed
                   | TableUnparseable Text -- ^ The table content cannot be parsed
                   deriving (Show)

-- | The different errors that the Lighstreamer server can throw
data RealTimeError = InvalidLogin
                   | UnavailableAdapterSet
                   | LicensedMaxSessionsReached
                   | ConfiguredMaxSessionsReached
                   | ConfiguredMaxServerLoadReached
                   | NewSessionsBlocked
                   | StreamingUnavailable
                   | MetadataAdapterError
                   | ClientVersionNotSupported
                   | DataAdapterUnknown
                   | TableNotFound
                   | BadItemGroupName
                   | BadItemGroupNameForSchema
                   | BadFieldSchemaName
                   | SubscriptionModeNotAllowed
                   | BadSelectorName
                   | NoUnfilteredDispatchFreq
                   | NoUnfilteredDispatchPre
                   | NoUnfilteredDispatch
                   | RawModeNotAllowed
                   | SubscriptionsNotAllowed
                   | SubscriptionRefused
                   | SessionDataUnreadable Text
                   | Other String
                   deriving (Eq, Show)


