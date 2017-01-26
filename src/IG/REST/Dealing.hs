-- | Functions for sending requests to the Dealing endpoint of the API
module Dealing where

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import IG
import IG.REST

-- | Obtain a DealConfirmation for a given reference. 
confirms a@(AuthHeaders _ _ isLogin _) ref = undefined


-- | Return all open positions for the active account
allPositions a@(AuthHeaders _ _ isLogin _) = undefined


-- | Return an open position for the active account by deal identifier
position a@(AuthHeaders _ _ isLogin _) id = undefined


-- | Close one or more open positions
closePositions a@(AuthHeaders _ _ isLogin _) = undefined


-- | Create a new position. The outcome of this action needs is ascertained using
-- @confirms
createPosition a@(AuthHeaders _ _ isLogin _) = undefined


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
