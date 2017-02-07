{-# LANGUAGE OverloadedStrings #-}
-- | Functions for sending requests to the Dealing endpoint of the API
module IG.REST.Dealing where

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time
import GHC.Generics
import IG
import IG.REST
import IG.REST.Dealing.Types
import Network.Wreq

confirmPath = "gateway/deal/confirms/"

-- | Obtain a DealConfirmation for a given reference. 
confirms :: AuthHeaders -> Text -> IO (Either ApiError DealConfirmation)
confirms a@(AuthHeaders _ _ _ isLogin) ref = do
  let opts = buildHeaders "1" a
  let url = Text.unpack $ host isLogin <> confirmPath <> "/" <> ref
  apiRequest (getWith opts url)
         
  
-- | Return all open positions for the active account
allPositions :: AuthHeaders -> IO (Either ApiError [PositionData])
allPositions a@(AuthHeaders _ _ _ isLogin) = do
  let opts = buildHeaders "2" a
  let positionsPath = "positions"
  let url = Text.unpack $ host isLogin <> positionsPath
  apiRequest $ getWith opts url


-- | Return an open position for the active account by deal identifier
position a@(AuthHeaders _ _ isLogin _) id = undefined


-- | Close one or more open positions
closePositions a@(AuthHeaders _ _ isLogin _) = undefined


-- | Create a new position. The outcome of this action needs is ascertained using
-- @confirms
createPosition :: AuthHeaders -> PositionRequest -> IO (Either ApiError DealReference)
createPosition a@(AuthHeaders _ _ _ isLogin ) positionRequest = do
  let opts = buildHeaders "2" a
  let otcPath = "/positions/otc"
  let url = Text.unpack $ host isLogin <> otcPath
  apiRequest $ postWith opts url (toJSON positionRequest)


-- | Update an open position
updatePosition a@(AuthHeaders _ _ isLogin _) id = undefined


-- | Return a list of all open sprint market positions for the active account
sprintPositions a@(AuthHeaders _ _ isLogin _) = undefined


-- | Create a sprint market position
createSprintPosition a@(AuthHeaders _ _ isLogin _) = undefined


-- | Return all open working orders for the active account
workingOrders a@(AuthHeaders _ _ isLogin _) = undefined


-- | Create a new working order for the active account
createWorkingOrders a@(AuthHeaders _ _ isLogin _) = undefined


-- | Delete an OTC working order
deleteWorkingOrder a@(AuthHeaders _ _ isLogin _) id = undefined


-- | Update a working order
updateWorkingOrder a@(AuthHeaders _ _ isLogin _) id = undefined
