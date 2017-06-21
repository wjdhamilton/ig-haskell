module IG.Realtime.Types where

import Data.Char
import Data.List as List
import Data.List.Split
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import IG
import Flow
import Network.Wreq

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
encodeProperty :: ControlProperty  -> FormParam
encodeProperty (SessionId s)       = "LS_session" := s
encodeProperty (Table n)           = "LS_table" := n
encodeProperty (Op o)              = "LS_op" := encode o
encodeProperty (DataAdapter n)     = "LS_data_adapter" := n
encodeProperty (Selector s)        = "LS_selector" := s
encodeProperty (Mode m)            = "LS_mode" := encode m
encodeProperty (ReqBufferSize n)   = "LS_requested_buffer_size" := show n
encodeProperty (ReqMaxFrequency f) = "LS_requested_max_frequency" := encode f


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
encodeSchema s@(Market _ fs)        = [schemaId s , schemaParams fs]
encodeSchema s@(SprintMarket _ fs)  = [schemaId s , schemaParams fs]
encodeSchema s@(Account _ fs)       = [schemaId s, schemaParams fs]
encodeSchema s@(Trade _ fs)         = [schemaId s, schemaParams fs]
encodeSchema s@(Chart _ _ fs)       = [schemaId s, schemaParams fs]
encodeSchema s@(ChartTick _ fs)     = [schemaId s, schemaParams fs]


schemaId :: Schema -> FormParam
schemaId (Market e _) = "LS_id" := "MARKET:" <> e
schemaId (SprintMarket e _) = "LS_id" := "MARKET:" <> e
schemaId (Account id _) = "LS_id" := "ACCOUNT:" <> id
schemaId (Trade id _) = "LS_id" := "TRADE:" <> id
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
  encode x       = Text.toUpper . Text.pack $ show x


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


class (Eq a, Ord a, Show a) => ControlAttribute a where
  encode :: a -> Text
  encode = Text.toUpper . snakeCase . toText


toText :: Show a => a -> Text
toText = Text.pack . show


snakeCase :: Text -> Text
snakeCase t =  Text.pack . tail . snake . Text.unpack $ t
  where snake []     = []
        snake (c:cs) = if isUpper c then '_' : c : snake cs
                                    else c : snake cs
