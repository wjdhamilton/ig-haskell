{-# LANGUAGE DeriveGeneric #-}
module IG.REST where

import Control.Lens
import Data.Aeson
import Data.ByteString.Lazy as BL
import Data.Monoid
import Data.String.Conversions
import Data.Text as Text
import Data.Time
import GHC.Generics
import IG
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wreq


(</>) :: Text -> Text -> Text
(</>) a b = a <> "/" <> b


restPathSegment :: Text
restPathSegment = "gateway" </> "deal"


-- | Encapsulates the Client Security Token and Account Session access token. 
-- These are both required for accessing the api and are acquired using @login
data AuthHeaders = AuthHeaders { cst :: Text -- ^ The Client Security Token
                               , securityToken :: Text -- ^ The account session security access token
                               , apiToken :: Text
                               , isDemo :: Bool
                               } deriving (Eq, Show)


v1 :: AuthHeaders -> Options
v1 = buildHeaders "1"


v2 :: AuthHeaders -> Options
v2 = buildHeaders "2"


v3 :: AuthHeaders -> Options
v3 = buildHeaders "3"


buildHeaders :: Text -> AuthHeaders -> Options
buildHeaders version (AuthHeaders c x k _) = 
  baseHeaders version k & header "CST" .~ [cs c]
                        & header "X-SECURITY-TOKEN" .~ [cs x]


baseHeaders :: Text -> Text -> Options
baseHeaders v key = defaults & header "Accept" .~ ["application/json", "charset=UTF-8"]
                             & header "Content-Type" .~ [ "application/json" ]
                             & header "Version" .~ [ cs v ]
                             & header "X-IG-API-KEY" .~ [cs key]
                             & manager .~ Left (tlsManagerSettings)
                             & checkResponse .~ (Just $ \_ _  -> return ()) -- TODO Check what this actually does


-- | Takes an IO call to the API and returns an instance of Either containing
-- of Left ApiError or Right TheRequestedType
apiRequest :: (FromJSON a) => IO (Response BL.ByteString) -> IO (Either ApiError a)
apiRequest request = do
  response <- request
  let body = response ^. responseBody
  case response ^. responseStatus ^. statusCode of
       200 -> do
         case eitherDecode body of
              Left e -> return $ Left (BadPayload $ cs e)
              Right r -> return $ Right r

       _ -> do return $ Left (decodeError body)


-- Some universal types that appear in various places

utcDateTimeFormat :: String
utcDateTimeFormat = iso8601DateFormat $ Just "%H:%M:%S%Q"

formats :: [String]
formats = [ "%H:%M:%S%Q", "%H:%M" ]

dateFormats :: [String]
dateFormats = Prelude.map (iso8601DateFormat . Just) formats

data UTCDate = UTCDate UTCTime
             deriving (Eq, Ord, Show)


instance FromJSON UTCDate where
  parseJSON = withText "UTCDate" $ \d ->
    case tryFormats dateFormats d of
         Nothing -> fail $ cs ("Could not parse" <> d)
         Just date -> return $ UTCDate date


instance ToJSON UTCDate where
  toJSON (UTCDate d) = String formatted
    where formatted = cs $ formatTime locale format d
          locale = defaultTimeLocale
          format = "%Y/%m/%dT%H:%M:%S"


-- | Represents a time point as returned by the API
newtype DealTime = DealTime UTCTime 
              deriving (Eq, Ord, Show)

-- TODO: Why not add this to the list of available formats and change the 
-- datatype to UTCDate
instance FromJSON DealTime where 

  parseJSON = withText "DealTime" $ \t -> 
    case time ( cs t ) of
         Just tm -> return $ DealTime tm
         Nothing -> fail $ cs t
    where time = parseTimeM True locale "%X" 
          locale = defaultTimeLocale


tryFormats :: [String] -> Text -> Maybe UTCTime
tryFormats [] _ = Nothing
tryFormats (f:fs) t = case parseTimeM True defaultTimeLocale f (cs t) of
                           Just date -> Just  date
                           Nothing -> tryFormats fs t


-- ^ Specifies the expiry of the instrument which you wish to purchase. The date
-- (and sometimes time) at which a spreadbet or CFD will automatically close 
-- against some predefined market value should the bet remain open beyond its 
-- last dealing time. Some CFDs do not expire, and have an expiry of '-'. eg 
-- DEC-14, or DFB for daily funded bets
data InstrumentExpiry = DFB
                      | None
                      | Expires Text Text
                      deriving (Eq, Ord, Show)

instance ToJSON InstrumentExpiry where

  toJSON (Expires m d) = String $ m <> "-" <> d
  toJSON None = String "-"
  toJSON x = String . cs . show $ x

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
             (x:_) -> fail . cs $ "Only one part of expiry data provided: " <> x

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
                    | UNKNOWN -- ^ Unknown
                    deriving (Eq, Ord, Generic, Show)

instance FromJSON InstrumentType 

