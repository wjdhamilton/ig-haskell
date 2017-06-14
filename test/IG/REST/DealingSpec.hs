-- | Tests the dealing module. Note that none of the "create" tests assert
-- that the deal has been accepted by the api, simply that the create action 
-- was successful
module IG.REST.DealingSpec (spec) where

import Control.Monad.Trans
import Control.Monad.Trans.Either
import Data.Either
import Data.Monoid
import Flow
import IG
import IG.REST
import IG.REST.Dealing
import IG.REST.Dealing.Types
import IG.REST.Login
import Test.Hspec

-- | Entry point for specs
spec :: Spec
spec = do
  (headers, loginResponse) <- runIO $ loginToApi True
  afterAll_ (closeAll headers) (descriptions headers)


descriptions :: AuthHeaders -> Spec
descriptions headers = do
  describe "createPosition"       $ createPositionSpec headers
  describe "confirms"             $ confirmsSpec headers
  describe "allPositions"         $ allPositionsSpec headers
  describe "closePosition"        $ closePositionSpec headers
  describe "updatePosition"       $ updatePositionSpec headers
  describe "workingOrders"        $ workingOrdersSpec headers
  describe "createWorkingOrders"  $ createWorkingOrderSpec headers
  describe "deleteWorkingOrder"   $ deleteWorkingOrderSpec headers


createPositionSpec :: AuthHeaders -> Spec
createPositionSpec headers = 
  it "should create the new position" $ do
    response <- createPosition headers minimalPositionRequest
    isRight response `shouldBe` True


defaultEpic = "IX.D.FTSE.DAILY.IP"


minimalPositionRequest :: PositionRequest
minimalPositionRequest = PositionRequest { currencyCode = "GBP"
                                         , dealReference = Nothing
                                         , direction = BUY
                                         , epic = defaultEpic
                                         , expiry = DFB
                                         , forceOpen = True
                                         , guaranteedStop = False
                                         , level = Nothing
                                         , limitDistance = Nothing
                                         , limitLevel = Nothing
                                         , orderType = MARKET
                                         , quoteId = Nothing
                                         , size = 1
                                         , stopLevel = Nothing
                                         , timeInForce = FILL_OR_KILL
                                         , trailingStop = False
                                         , trailingStopIncrement = Nothing
                                         }


confirmsSpec :: AuthHeaders -> Spec
confirmsSpec headers =
  it "should retrieve the status of the deal" $ do
    dealRef <- createPosition headers minimalPositionRequest
    case dealRef of
         Left e -> error $ "Create position failed: " <> show e
         Right ref -> do
           confirmation <- confirms headers ref
           isRight confirmation `shouldBe` True


allPositionsSpec :: AuthHeaders -> Spec
allPositionsSpec headers =
  it "should retrieve a list of all the currently open positions" $ do
    positions <- allPositions headers
    isRight positions `shouldBe` True


closePositionSpec :: AuthHeaders -> Spec
closePositionSpec headers =
  it "should close a position correctly" $ do
    closeRef <- runEitherT $ do
      eDealRef <- lift $ createPosition headers minimalPositionRequest
      ref <- hoistEither eDealRef
      confirmation <- lift $ confirms headers ref
      conf <- hoistEither confirmation
      let id = dealId (conf :: DealConfirmation)
      p <- lift $ positionDetails headers id
      pos <- hoistEither p
      let opts = CloseOptions Nothing MARKET Nothing Nothing Nothing Nothing Nothing
      let cpr = toClosePositionRequest pos opts
      lift $ closePosition headers cpr
    isRight closeRef `shouldBe` True


updatePositionSpec :: AuthHeaders -> Spec
updatePositionSpec headers = 
  it "should update the position correctly" $ do
    response <- runEitherT $ do
      eDealRef <- lift $ createPosition headers minimalPositionRequest
      dealRef <- hoistEither eDealRef
      eConf <- lift $ confirms headers dealRef
      confirmation <- hoistEither eConf
      let id = dealId (confirmation :: DealConfirmation)
      ePos <- lift $ positionDetails headers id
      pos <- hoistEither ePos
      let p = position (pos :: PositionData)
      let l = level (p :: Position)
      let stopLevel = Just $ l - 10
      let updateOpts = PositionUpdateRequest Nothing stopLevel (Just False) Nothing Nothing
      lift $ updatePosition headers id updateOpts
    isRight response `shouldBe` True


workingOrdersSpec :: AuthHeaders -> Spec 
workingOrdersSpec headers = 
  it "should download all the working orders" $ do
    orders <- getWorkingOrders headers
    isRight orders `shouldBe` True


-- | Checks that a working order is correctly submitted, but does NOT check
-- that the working order is accepted. These are two different matters!
createWorkingOrderSpec :: AuthHeaders -> Spec 
createWorkingOrderSpec headers = 
 it "should create a new working order" $ do
   cr <- createWorkingOrder headers minimalWorkingOrder
   isRight cr `shouldBe` True 


minimalWorkingOrder = WorkingOrderRequest { currencyCode = "GBP" 
                                          , dealReference = Nothing 
                                          , direction = BUY
                                          , epic = defaultEpic
                                          , expiry = DFB
                                          , forceOpen = False
                                          , goodTillDate = Nothing
                                          , guaranteedStop = False
                                          , level = 7500
                                          , limitDistance = Nothing
                                          , limitLevel = Nothing
                                          , size = 1
                                          , stopDistance = Nothing
                                          , stopLevel = Nothing
                                          , timeInForce = GOOD_TILL_CANCELLED
                                          , woType = LIMIT
                                          } 


-- | This is a bit difficult to test programmatically since creating a valid
-- working order is complicated by the fact that the level value must be valid,
-- and who knows what level the market is at right now? Yes, it works.
deleteWorkingOrderSpec :: AuthHeaders -> Spec
deleteWorkingOrderSpec h = do
  it "should delete the working order" $ do
    deleted <- runEitherT $ do
      eCreated <- lift $ createWorkingOrder h minimalWorkingOrder
      hoistEither eCreated
      eOrders <- lift $ getWorkingOrders h
      orders <- hoistEither eOrders
      let order = head . workingOrders $ orders
      let id = dealId (workingOrderData order :: WorkingOrder)
      lift $ deleteWorkingOrder h id
    isRight deleted `shouldBe` True


closeAll :: AuthHeaders -> IO ()
closeAll h = do
  eOpenPositions <- allPositions h
  case eOpenPositions of
       Left e -> print e
       Right openPositions -> do
         let posits = positions openPositions
         mapM (closeRequest h) posits
         return ()


closeRequest :: AuthHeaders -> PositionData -> IO (Either ApiError DealReference)
closeRequest h pos = closePosition h $ toClosePositionRequest pos options
  where options = CloseOptions Nothing MARKET Nothing Nothing Nothing Nothing Nothing
