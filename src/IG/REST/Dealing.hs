-- | Functions for sending requests to the Dealing endpoint of the API
{-# LANGUAGE NamedFieldPuns #-}
module IG.REST.Dealing where

import Control.Lens
import Data.Aeson
import Data.Maybe
import Data.Monoid
import Data.String.Conversions
import Data.Text (Text)
import IG
import IG.REST
import IG.REST.Dealing.Types
import Network.Wreq

-- | Obtain a DealConfirmation for a given reference. 
confirms :: AuthHeaders -> DealReference -> IO (Either ApiError DealConfirmation)
confirms a@(AuthHeaders _ _ _ isDemo) ref = do
  let opts = v1 a
  let reference = dealReference (ref :: DealReference)
  let confirmPath = restPathSegment </> "confirms" </> mempty
  let url = cs $ host isDemo </> confirmPath </> reference
  apiRequest (getWith opts url)
         
  
-- | Return all open positions for the active account
allPositions :: AuthHeaders -> IO (Either ApiError PositionsResponse)
allPositions a@(AuthHeaders _ _ _ isDemo) = do
  let opts = v2 a
  let positionsPath = "positions"
  let url = cs $ host isDemo </> restPathSegment </> positionsPath
  apiRequest $ getWith opts url


-- | Return an open position for the active account by deal identifier
positionDetails :: AuthHeaders -> Text -> IO (Either ApiError PositionData)
positionDetails a@(AuthHeaders _ _ _ isDemo) id = do
  let opts = v2 a
  let url = cs $ host isDemo </> restPathSegment </> "positions" </> id
  apiRequest $ getWith opts url


otcPath :: Bool -> String
otcPath isDemo = cs $ host isDemo </> restPathSegment </> "positions/otc"

-- | Close one position
closePosition :: AuthHeaders -> ClosePositionRequest -> IO (Either ApiError DealReference)
closePosition a@(AuthHeaders _ _ _ isDemo) cpr = do
  let opts = v1 a & header "_method" .~ ["DELETE"]
  let url = otcPath isDemo
  let payload = toJSON cpr
  apiRequest $ postWith opts url payload


-- | Takes an instance of PositionData and one of CloseOptions and produces a 
-- ClosePositionRequest that closes the position insofar as the CloseOptions 
-- permit
-- TODO Would it not be more useful if CloseOptions were wrapped in a maybe
-- and if Nothing is passed in then defaults apply?
toClosePositionRequest :: PositionData -> CloseOptions -> ClosePositionRequest
toClosePositionRequest PositionData {position, market} opts =
  ClosePositionRequest { dealId = dealId (position :: Position)
                       , epic = epic (opts :: CloseOptions)
                       , expiry = expiry (opts :: CloseOptions)
                       , direction = dir
                       , size = dealSize
                       , level = dealLevel
                       , orderType = orderType (opts :: CloseOptions)
                       , timeInForce = timeInForce (opts :: CloseOptions)
                       , quoteId = quoteId (opts :: CloseOptions)
                       }
  where 
        dir = if direction (position :: Position) == BUY then SELL else BUY
        dealSize = fromMaybe (size (position :: Position)) (size (opts :: CloseOptions))
        dealLevel = level (opts :: CloseOptions)


-- | Create a new position. The outcome of this action needs is ascertained using
-- @confirms
createPosition :: AuthHeaders -> PositionRequest -> IO (Either ApiError DealReference)
createPosition a@(AuthHeaders _ _ _ isDemo ) positionRequest = do
  let opts = v2 a
  apiRequest $ postWith opts (otcPath isDemo ) (toJSON positionRequest)


-- | Update an open position
updatePosition :: AuthHeaders -> Text -> PositionUpdateRequest -> IO (Either ApiError DealReference)
updatePosition a@(AuthHeaders _ _ _ isDemo) id req = do
  let opts = v2 a
  let url = otcPath isDemo <> "/" <> cs id
  apiRequest $ putWith opts url (toJSON req)


-- | Return all open working orders for the active account
getWorkingOrders :: AuthHeaders -> IO (Either ApiError WorkingOrdersResponse)
getWorkingOrders a@(AuthHeaders _ _ _ isDemo) = do
  let opts = v2 a
  let url = cs $ host isDemo </> restPathSegment </> "workingorders"
  apiRequest $ getWith opts url


otcWorkingOrderPath :: Bool -> Maybe Text -> Text
otcWorkingOrderPath isDemo mId =
  host isDemo </> restPathSegment </> "workingorders" </> "otc" </> id
  where id = fromMaybe "" mId


-- | Create a new working order for the active account
createWorkingOrder :: AuthHeaders -> WorkingOrderRequest -> IO (Either ApiError DealReference)
createWorkingOrder a@(AuthHeaders _ _ _ isDemo) req = do
  let opts = v2 a
  let url = cs $ otcWorkingOrderPath isDemo Nothing
  apiRequest $ postWith opts url (toJSON req)


-- | Delete an OTC working order
deleteWorkingOrder :: AuthHeaders -> Text -> IO (Either ApiError DealReference)
deleteWorkingOrder a@(AuthHeaders _ _ _ isDemo) id = do
  let opts = v2 a
  let url = cs $ otcWorkingOrderPath isDemo (Just id)
  apiRequest $ deleteWith opts url


-- | Update a working order
updateWorkingOrder :: AuthHeaders -> Text -> WorkingOrderUpdate -> IO (Either ApiError DealReference)
updateWorkingOrder a@(AuthHeaders _ _ _ isDemo) id update = do
  let opts = v2 a
  let url = cs $ otcWorkingOrderPath isDemo (Just id)
  apiRequest $ putWith opts url (toJSON update)
