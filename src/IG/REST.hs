{-# LANGUAGE DeriveGeneric #-}
module IG.REST where

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString.Lazy as BL
import Data.Either.Unwrap
import Data.Maybe
import Data.Monoid
import Data.Text as Text
import Data.Text.Encoding as TE
import Data.Time
import GHC.Generics
import IG
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types hiding (statusCode)
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


v1 = buildHeaders "1"


v2 = buildHeaders "2"


v3 = buildHeaders "3"


buildHeaders :: Text -> AuthHeaders -> Options
buildHeaders version (AuthHeaders c x k _) = 
  baseHeaders version k & header "CST" .~ [TE.encodeUtf8 c]
                        & header "X-SECURITY-TOKEN" .~ [TE.encodeUtf8 x]


baseHeaders :: Text -> Text -> Options
baseHeaders v key = defaults & header "Accept" .~ ["application/json", "charset=UTF-8"]
                             & header "Content-Type" .~ [ "application/json" ]
                             & header "Version" .~ [ TE.encodeUtf8 v ]
                             & header "X-IG-API-KEY" .~ [TE.encodeUtf8 key]
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
              Left e -> return $ Left (BadPayload $ Text.pack e)
              Right r -> return $ Right r

       _ -> do return $ Left (decodeError body)


-- Some universal types that appear in various places

utcDateTimeFormat :: String
utcDateTimeFormat = iso8601DateFormat $ Just "%H:%M:%S%Q"

formats = [ "%H:%M:%S%Q", "%H:%M", "%X" ]

dateFormats = Prelude.map (iso8601DateFormat . Just) formats

data IGDate = IGDate UTCTime
             deriving (Show)


instance FromJSON IGDate where
  parseJSON = withText "IGDate" $ \d ->
    case tryFormats dateFormats d of
         Nothing -> fail $ Text.unpack ("Could not parse" <> d)
         Just date -> return $ IGDate date


instance ToJSON IGDate where
  toJSON (IGDate d) = String formatted
    where formatted = Text.pack $ formatTime locale format d
          locale = defaultTimeLocale
          format = "%Y/%m/%dT%H:%M:%S"


toUTCTime :: IGDate -> UTCTime
toUTCTime (IGDate d) = d


-- | Alias for IGDate that indicates that only the time parts of the date will
-- have been completed
type IGTime = IGDate

tryFormats :: [String] -> Text -> Maybe UTCTime
tryFormats [] t = Nothing
tryFormats (f:fs) t = case parseTimeM True defaultTimeLocale f (Text.unpack t) of
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
                    deriving (Generic, Show)

instance FromJSON InstrumentType 

