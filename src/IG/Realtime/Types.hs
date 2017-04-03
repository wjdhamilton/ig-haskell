module IG.Realtime.Types where

import Data.Char
import Data.List as List
import Data.List.Split
import Data.Monoid
import Data.Text (Text)
import IG
import Flow
import qualified Data.Text as Text
import Network.Wreq

-- | The different attributes that are understood by the IG Realtime system
data ControlProperty = SessionId Text
                     | Table Int
                     | Op StreamOp
                     | DataAdapter Text
                     | Selector Text
                     | Mode SubMode
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
              deriving (Show)

instance ControlAttribute StreamOp where
  encode = Text.toLower . toText


data SubMode = Merge
             | Distinct
             deriving (Show)

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
               deriving (Show)


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
                  deriving (Show)


instance ControlAttribute MarketFields


-- | Fields that can be returned for a SprintMarket
data SprintFields = StrikePrice
                  | SprintMarketState
                  | Odds
                  deriving (Show)


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
                   deriving (Show)

instance ControlAttribute AccountFields


-- | Fields that can be returned 
data TradeFields = Confirms
                 | Opu
                 | Wou
                 deriving (Show)


instance ControlAttribute TradeFields


data ChartFields = Ltv
                 | Ttv
                 | Utm
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
                 deriving (Show)

instance ControlAttribute ChartFields


data TimeSlice = Second | Minute1 | Minute5 | Hour deriving (Show)


instance ControlAttribute TimeSlice


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
                deriving (Show)


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
                 deriving (Show)


instance ControlAttribute SnapshotAtt where
  encode (UseSnapshot perhaps) = Text.toLower . toText $ perhaps
  encode (Length n)            = toText n


class (Show a) => ControlAttribute a where
  encode :: a -> Text
  encode = Text.toUpper . snakeCase . toText


toText :: Show a => a -> Text
toText = Text.pack . show


snakeCase :: Text -> Text
snakeCase t = t
            |> Text.toLower
            |> Text.unpack
            |> splitWhen isUpper
            |> intercalate "_"
            |> Text.pack
