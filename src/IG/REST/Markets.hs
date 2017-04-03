-- | Provides functions and datatypes for interacting with the /market endpoint
-- | IG Api.

module IG.REST.Markets where

import Control.Lens hiding ((|>), from, to)
import Data.Aeson
import Data.Aeson.Types hiding (Options)
import Data.List as List
import Data.ByteString.Char8 as BS
import Data.ByteString.Lazy.Char8 as BL
import Data.Monoid 
import Data.Text as Text
import Data.Text.Encoding as TE
import Data.Time
import Flow
import IG
import IG.REST
import IG.REST.Markets.Types as Types
import Network.Wreq
import Prelude hiding (max)

marketUrl :: Bool -> Text
marketUrl isDemo =
  host isDemo </> restPathSegment 

-- | Get the subnodes of a given node in the market navigation hierarchy
navigateMarkets :: AuthHeaders -> Maybe Node -> IO (Either ApiError MarketData)
navigateMarkets a@(AuthHeaders _ _ _ isDemo) n = do
  let opts = v1 a
  let suffix = case n of
                    Nothing -> mempty
                    Just (Node id _) -> id
  let url = Text.unpack $ marketUrl isDemo </> "marketnavigation" </> suffix 
  apiRequest $ getWith opts url


-- | Download the details of a given set of markets. There is in fact a separate
-- endpoint that allows the user to download the details of a single market, 
-- but since that can be achieved here anyway it has not been included. 
markets :: AuthHeaders -> [Epic] -> IO (Either ApiError [MarketDetails])
markets a@(AuthHeaders _ _ _ isDemo) epics = do
  let epicsQuery = Text.concat . List.intersperse "," $ epics
  let opts = v2 a & param "epics" .~ [ epicsQuery]
  let url = Text.unpack $ marketUrl isDemo </> "markets" 
  response <- apiRequest $ getWith opts url :: IO (Either ApiError Value)
  case response of
       Left e -> return $ Left e
       Right r -> 
         let 
           decodeMarkets :: Value -> Parser [MarketDetails]
           decodeMarkets = withObject "markets" $ \o -> o .: "marketDetails" 
         in
           case parseEither decodeMarkets r of
                Left parseError -> return (Left . BadPayload . Text.pack $ parseError)
                Right details -> return $ Right details


historicalData :: AuthHeaders -> Epic -> HistoryOpts -> IO (Either ApiError HistoricalPrices)
historicalData a@(AuthHeaders _ _ _ isDemo) e hOpts = do
  let baseOpts = v3 a
  let timeF = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" :: UTCTime -> String
  let opts = baseOpts 
           |> histOpt "from"        (from hOpts) (\x -> Text.pack $ timeF x)
           |> histOpt "to"          (to hOpts) (\x -> Text.pack $ timeF x)
           |> histOpt "max"         (max (hOpts :: HistoryOpts)) (Text.pack . show)
           |> histOpt "pageSize"    (pageSize (hOpts:: HistoryOpts)) (Text.pack . show)
           |> histOpt "resolution"  (resolution hOpts) (Text.pack . show)
  let url = Text.unpack $ marketUrl isDemo </> "prices" </> e 
  apiRequest $ getWith opts url


histOpt :: Text -> Maybe a -> (a -> Text) -> Options -> Options
histOpt k value f opts = case value of
                         Nothing -> opts
                         Just v -> opts & param k .~ [f v]
